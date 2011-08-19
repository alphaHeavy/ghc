/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1994-2000.
 *
 * Heap printer
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "rts/Bytecodes.h"  /* for InstrPtr */

#include "PrinterXml.h"
#include "Printer.h"
#include "RtsUtils.h"

#include <string.h>
#include <setjmp.h>
#include <mach/mach.h>

#include <libxml/xmlwriter.h>

#ifdef DEBUG

#include "Disassembler.h"
#include "Apply.h"

/* --------------------------------------------------------------------------
 * local function decls
 * ------------------------------------------------------------------------*/

typedef rtsBool (*HEAP_FILTER)(const void*, const StgInfoTable*, const StgClosure*);

static void printStdObjPayloadXml( xmlTextWriterPtr writer, const StgClosure *obj );
static void printClosureXml(xmlTextWriterPtr writer, HEAP_FILTER filter, const void* context, const StgClosure *obj);

/* --------------------------------------------------------------------------
 * Printer
 * ------------------------------------------------------------------------*/

static void
writePointerAttribute(xmlTextWriterPtr writer, const char *name, const void* value)
{
    if (value != 0) {
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)name, "%p", value);
    }
}

STATIC_INLINE void
printStdObjHdrXml (xmlTextWriterPtr writer, const StgClosure *obj, const char* tag)
{
    xmlTextWriterWriteAttribute(writer, (const xmlChar *)"tag", (const xmlChar *)tag);
    writePointerAttribute(writer, "info", ((StgPtr)obj->header.info));
#ifdef PROFILING
    xmlTextWriterWriteAttribute(writer, "label", obj->header.prof.ccs->cc->label);
#endif
}

static void
printInfoTablePayloadXml (xmlTextWriterPtr writer, const StgInfoTable* info, const StgClosurePtr* payload)
{
    xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"ptrs", "%lu", (lnat)info->layout.payload.ptrs);
    xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"nptrs", "%lu", (lnat)info->layout.payload.nptrs);
    StgWord i;
    for (i = 0; i < info->layout.payload.ptrs; ++i) {
        if (payload[i] != 0) {
            xmlTextWriterStartElement(writer, (const xmlChar *)"ptr");
            xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"addr", "%p", payload[i]);
            xmlTextWriterEndElement(writer); // </ptr>
        }
    }

    StgWord j;
    for (j = 0; j < info->layout.payload.nptrs; ++j) {
        if (payload[i+j] != 0) {
            xmlTextWriterStartElement(writer, (const xmlChar *)"nptr");
            xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"val", "%p", payload[i+j]);
            xmlTextWriterEndElement(writer); // </ptr>
        }
    }
}

static void
printStdObjPayloadXml (xmlTextWriterPtr writer, const StgClosure *obj)
{
    printInfoTablePayloadXml(writer, get_itbl(obj), obj->payload);
}

static void
printThunkPayloadXml (xmlTextWriterPtr writer, const StgThunk *obj)
{
    printInfoTablePayloadXml(writer, get_itbl(obj), obj->payload);
}

static void
printThunkObjectXml (xmlTextWriterPtr writer, const StgThunk *obj, const char* tag)
{
    printStdObjHdrXml(writer, (const StgClosure *)obj, tag);
    printThunkPayloadXml(writer, obj);
}

extern void
printClosureXml (xmlTextWriterPtr writer, HEAP_FILTER filter, const void* context, const StgClosure *obj)
{
    obj = UNTAG_CLOSURE((StgClosure *)obj);

    const StgInfoTable* info;
    info = get_itbl(obj);

    if (filter(context, info, obj) == rtsTrue) {
        return;
    }

    xmlTextWriterStartElement(writer, (const xmlChar *)closure_type_names[info->type]);
    writePointerAttribute(writer, "ptr", obj);

    switch (info->type) {
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC: {
        StgWord i, j;
        const StgConInfoTable* con_info = get_con_itbl(obj);
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"ptrs", "%lu", (lnat)info->layout.payload.ptrs);
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"nptrs", "%lu", (lnat)info->layout.payload.nptrs);

        const char* con = GET_CON_DESC(con_info);
        if (con != 0 && strlen(con) > 0) {
            xmlTextWriterWriteAttribute(writer, (const xmlChar *)"con", (const xmlChar *)con);
        }

        for (i = 0; i < info->layout.payload.ptrs; ++i) {
            if (obj->payload[i] != 0) {
                if (obj->payload[i] != 0) {
                    xmlTextWriterStartElement(writer, (const xmlChar *)"ptr");
                    xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"addr", "%p", obj->payload[i]);
                    xmlTextWriterEndElement(writer); // </ptr>
                }
            }
        }
        for (j = 0; j < info->layout.payload.nptrs; ++j) {
            if (obj->payload[i+j] != 0) {
                if (obj->payload[i+j] != 0) {
                    xmlTextWriterStartElement(writer, (const xmlChar *)"nptr");
                    xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"val", "%p", obj->payload[i+j]);
                    xmlTextWriterEndElement(writer); // </ptr>
                }
            }
        }
        break;
    }

    case FUN:
    case FUN_1_0:
    case FUN_0_1:
    case FUN_1_1:
    case FUN_0_2:
    case FUN_2_0:
    case FUN_STATIC:
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"arity", "%d", itbl_to_fun_itbl(info)->f.arity);
        writePointerAttribute(writer, "info", ((StgPtr)obj->header.info));
#ifdef PROFILING
        xmlTextWriterWriteAttribute(writer, "label", obj->header.prof.ccs->cc->label);
#endif
        printStdObjPayloadXml(writer, obj);
        break;

    case PRIM:
        writePointerAttribute(writer, "info", ((StgPtr)obj->header.info));
        printStdObjPayloadXml(writer, obj);
        break;

    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_1_1:
    case THUNK_0_2:
    case THUNK_2_0:
    case THUNK_STATIC:
        /* ToDo: will this work for THUNK_STATIC too? */
#ifdef PROFILING
        printThunkObjectXml(writer, (const StgThunk *)obj, GET_PROF_DESC(info));
#else
        printThunkObjectXml(writer, (const StgThunk *)obj, "THUNK");
#endif
        break;

    case THUNK_SELECTOR:
        printStdObjHdrXml(writer, obj, "THUNK_SELECTOR");
        writePointerAttribute(writer, "selectee", ((StgSelector *)obj)->selectee);
        break;

    case BCO:
        // TODO: fix
        // xmlTextWriterWriteFormatAttribute(writer, "bytecode", "%p", ((StgSelector *)obj)->selectee);
        // disassemble((const StgBCO*)obj);
        break;

    case AP: {
        const StgAP* ap = (const StgAP*)obj;
        StgWord i;
        writePointerAttribute(writer, "fun", ap->fun);
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"args", "%lu", (lnat)ap->n_args);
        for (i = 0; i < ap->n_args; ++i) {
            if (ap->payload[i] != 0) {
                xmlTextWriterStartElement(writer, (const xmlChar *)"ptr");
                xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"addr", "%p", ((P_)ap->payload[i]));
                xmlTextWriterEndElement(writer); // </ptr>
            }
        }
        break;
    }

    case PAP: {
        const StgPAP* pap = (const StgPAP*)obj;
        StgWord i;
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"arity", "%d", pap->arity);
        writePointerAttribute(writer, "fun", (StgPtr)pap->fun);
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"args", "%lu", (lnat)pap->n_args);
        for (i = 0; i < pap->n_args; ++i) {
            if (pap->payload[i] != 0) {
                xmlTextWriterStartElement(writer, (const xmlChar *)"ptr");
                xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"addr", "%p", ((P_)pap->payload[i]));
                xmlTextWriterEndElement(writer); // </ptr>
            }
        }
        break;
    }

    case AP_STACK: {
        const StgAP_STACK* ap = (const StgAP_STACK*)obj;
        StgWord i;
        writePointerAttribute(writer, "fun", ap->fun);
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"size", "%lu", ap->size);
        for (i = 0; i < ap->size; ++i) {
            if (ap->payload[i] != 0) {
                xmlTextWriterStartElement(writer, (const xmlChar *)"ptr");
                xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"addr", "%p", ((P_)ap->payload[i]));
                xmlTextWriterEndElement(writer); // </payload>
            }
        }
        break;
    }

    case IND:
    case IND_PERM:
    case IND_STATIC:
    case BLACKHOLE:
        writePointerAttribute(writer, "indirectee", (StgPtr)((StgInd*)obj)->indirectee);

    case UPDATE_FRAME: {
        const StgUpdateFrame* u = (const StgUpdateFrame*)obj;
        writePointerAttribute(writer, "info", ((StgPtr)GET_INFO(u)));
        writePointerAttribute(writer, "updatee", ((StgPtr)u->updatee));
        break;
    }

    case CATCH_FRAME: {
        const StgCatchFrame* u = (const StgCatchFrame*)obj;
        writePointerAttribute(writer, "info", ((StgPtr)GET_INFO(u)));
        writePointerAttribute(writer, "handler", ((StgPtr)u->handler));
        break;
    }

    case UNDERFLOW_FRAME: {
        const StgUnderflowFrame* u = (const StgUnderflowFrame*)obj;
        writePointerAttribute(writer, "nextChunk", ((StgPtr)u->next_chunk));
        break;
    }

    case STOP_FRAME: {
        const StgStopFrame* u = (const StgStopFrame*)obj;
        writePointerAttribute(writer, "info", ((StgPtr)GET_INFO(u)));
        break;
    }

    case ARR_WORDS: {
        const StgArrWords* arr = (const StgArrWords *)obj;
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"bytes", "%u", (unsigned int)arr->bytes);
        xmlTextWriterWriteBase64(writer, (const char *)arr->payload, 0, arr->bytes);
        break;
    }

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN: {
        const StgMutArrPtrs* arr = (const StgMutArrPtrs *)obj;
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"size", "%lu", (lnat)arr->ptrs);
        break;
    }

    case MVAR_CLEAN:
    case MVAR_DIRTY: {
        const StgMVar* mv = (const StgMVar*)obj;
        writePointerAttribute(writer, "head", mv->head);
        writePointerAttribute(writer, "tail", mv->tail);
        writePointerAttribute(writer, "value", mv->value);
        break;
    }

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY: {
        const StgMutVar* mv = (const StgMutVar*)obj;
        writePointerAttribute(writer, "var", mv->var);
        break;
    }

    case WEAK: {
        const StgWeak* w = (const StgWeak*)obj;
        writePointerAttribute(writer, "key", w->key);
        writePointerAttribute(writer, "value", w->value);
        writePointerAttribute(writer, "finalizer", w->finalizer);
        writePointerAttribute(writer, "cfinalizer", w->cfinalizer);
        /* ToDo: chase 'link' ? */
        break;
    }

    case TSO: {
        const StgTSO* tso = (const StgTSO*)obj;
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"id", "%lu", (lnat)tso->id);
        break;
    }

    case STACK: {
        const StgStack* stack = (const StgStack*)obj;
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"size", "%lu", (lnat)stack->stack_size);
        break;
    }

    default:
        // no additional details to add
        break;
    }

    xmlTextWriterEndElement(writer); // </object>
}

#if 0
static StgPtr
printStackObj (xmlTextWriterPtr writer, StgPtr sp)
{
    const StgClosure* c = (const StgClosure*)(*sp);
    printPtr((StgPtr)*sp);
    if (c == (const StgClosure*)&stg_ctoi_R1p_info) {
        debugBelch("\t\t\tstg_ctoi_ret_R1p_info\n" );
    } else
    if (c == (const StgClosure*)&stg_ctoi_R1n_info) {
        debugBelch("\t\t\tstg_ctoi_ret_R1n_info\n" );
    } else
    if (c == (const StgClosure*)&stg_ctoi_F1_info) {
        debugBelch("\t\t\tstg_ctoi_ret_F1_info\n" );
    } else
    if (c == (const StgClosure*)&stg_ctoi_D1_info) {
        debugBelch("\t\t\tstg_ctoi_ret_D1_info\n" );
    } else
    if (c == (const StgClosure*)&stg_ctoi_V_info) {
        debugBelch("\t\t\tstg_ctoi_ret_V_info\n" );
    } else
    if (get_itbl(c)->type == BCO) {
        debugBelch("\t\t\t");
        debugBelch("BCO(...)\n");
    }
    else {
        debugBelch("\t\t\t");
        printClosure(writer, (const StgClosure*)(*sp));
    }
    sp += 1;

    return sp;
}

static void
printSmallBitmap (xmlTextWriterPtr writer, const StgPtr spBottom, const StgPtr payload, StgWord bitmap, nat size)
{
    nat i;

    for (i = 0; i < size; i++, bitmap >>= 1) {
        debugBelch("   stk[%ld] (%p) = ", (long)(spBottom-(payload+i)), payload+i);
        if ((bitmap & 1) == 0) {
            printPtr((P_)payload[i]);
            debugBelch("\n");
        } else {
            debugBelch("Word# %lu\n", (lnat)payload[i]);
        }
    }
}

static void
printLargeBitmap (xmlTextWriterPtr writer, const StgPtr spBottom, const StgPtr payload, const StgLargeBitmap* large_bitmap, nat size)
{
    StgWord bmp;
    nat i, j;

    i = 0;
    for (bmp=0; i < size; bmp++) {
        StgWord bitmap = large_bitmap->bitmap[bmp];
        j = 0;
        for (; i < size && j < BITS_IN(W_); j++, i++, bitmap >>= 1) {
            debugBelch("   stk[%lu] (%p) = ", (lnat)(spBottom-(payload+i)), payload+i);
            if ((bitmap & 1) == 0) {
                printPtr((P_)payload[i]);
                debugBelch("\n");
            } else {
                debugBelch("Word# %lu\n", (lnat)payload[i]);
            }
        }
    }
}

extern void
printStackChunk (xmlTextWriterPtr writer, StgPtr sp, const StgPtr spBottom)
{
    StgWord bitmap;
    const StgInfoTable *info;

    ASSERT(sp <= spBottom);
    for (; sp < spBottom; sp += stack_frame_sizeW((const StgClosure *)sp)) {
        info = get_itbl((const StgClosure *)sp);

        switch (info->type) {
        case UPDATE_FRAME:
        case CATCH_FRAME:
        case UNDERFLOW_FRAME:
        case STOP_FRAME:
            printClosure(writer, (const StgClosure*)sp);
            continue;

        case RET_DYN: {
            const StgRetDyn* r;
            StgPtr p;
            StgWord dyn;
            nat size;

            r = (const StgRetDyn *)sp;
            dyn = r->liveness;
            debugBelch("RET_DYN (%p)\n", r);

            p = (P_)(r->payload);
            printSmallBitmap(writer,
                             spBottom,
                             sp,
                             RET_DYN_LIVENESS(r->liveness),
                             RET_DYN_BITMAP_SIZE);
            p += RET_DYN_BITMAP_SIZE + RET_DYN_NONPTR_REGS_SIZE;

            for (size = RET_DYN_NONPTRS(dyn); size > 0; size--) {
                debugBelch("   stk[%ld] (%p) = ", (long)(spBottom-p), p);
                debugBelch("Word# %ld\n", (long)*p);
                p++;
            }

            for (size = RET_DYN_PTRS(dyn); size > 0; size--) {
                debugBelch("   stk[%ld] (%p) = ", (long)(spBottom-p), p);
                printPtr(p);
                p++;
            }
            continue;
        }

        case RET_SMALL:
            debugBelch("RET_SMALL (%p)\n", info);
            bitmap = info->layout.bitmap;
            printSmallBitmap(writer,
                             spBottom,
                             sp+1,
                             BITMAP_BITS(bitmap),
                             BITMAP_SIZE(bitmap));
            continue;

        case RET_BCO: {
            const StgBCO *bco;

            bco = ((const StgBCO *)sp[1]);

            debugBelch("RET_BCO (%p)\n", sp);
            printLargeBitmap(writer,
                             spBottom,
                             sp+2,
                             BCO_BITMAP(bco),
                             BCO_BITMAP_SIZE(bco));
            continue;
        }

        case RET_BIG:
            barf("todo");

        case RET_FUN: {
            const StgFunInfoTable *fun_info;
            const StgRetFun *ret_fun;

            ret_fun = (const StgRetFun *)sp;
            fun_info = get_fun_itbl(ret_fun->fun);
            debugBelch("RET_FUN (%p) (type=%d)\n", ret_fun->fun, fun_info->f.fun_type);
            switch (fun_info->f.fun_type) {
            case ARG_GEN:
                printSmallBitmap(writer,
                                 spBottom,
                                 sp+2,
                                 BITMAP_BITS(fun_info->f.b.bitmap),
                                 BITMAP_SIZE(fun_info->f.b.bitmap));
                break;
            case ARG_GEN_BIG:
                printLargeBitmap(writer,
                                 spBottom,
                                 sp+2,
                                 GET_FUN_LARGE_BITMAP(fun_info),
                                 GET_FUN_LARGE_BITMAP(fun_info)->size);
                break;
            default:
                printSmallBitmap(writer,
                                 spBottom,
                                 sp+2,
                                 BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]),
                                 BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]));
                break;
            }
            continue;
        }

        default:
            debugBelch("unknown object %d\n", info->type);
            barf("printStackChunk");
        }
    }
}

static void
printTSO (xmlTextWriterPtr writer, const StgTSO *tso)
{
    printStackChunk(writer,
                    tso->stackobj->sp,
                    tso->stackobj->stack+tso->stackobj->stack_size);
}
#endif

static jmp_buf jmpbuf;
__attribute__ ((noreturn)) static void
segvHandler (int signo)
{
    debugBelch("<<%s>>\n", strsignal(signo));
    siglongjmp(jmpbuf, 1);
}

static void
printHeapChunkXml (xmlTextWriterPtr writer, HEAP_FILTER filter, const void* context, const bdescr *bd)
{
    volatile StgPtr q;
    for ( ; bd; bd = bd->link) {
        for (q = bd->start; q < bd->free; ++q) {
            if (sigsetjmp(jmpbuf, 1) == 0)
            {
                for ( ; q < bd->free && *q == 0; ++q)
                    ; // skip over zeroed-out slop

                if (*q == 0) {
                    continue;
                }

                if (!LOOKS_LIKE_CLOSURE_PTR(q)) {
                    // debugBelch("%p found at %p, no closure at %p\n", p, q, r);
                    continue;
                }

                StgPtr end = q + closure_sizeW((StgClosure *)q);
                if (end >= bd->free) {
                    // debugBelch("%p found at %p, closure?", p, q);
                    continue;
                }

                printClosureXml(writer, filter, context, (const StgClosure *)q);
            }
        }
    }
}

static void
printHeapWithFilter (const char *fileName, HEAP_FILTER filter, const void* context)
{
    kern_return_t kret = task_set_exception_ports(
        mach_task_self(),
        EXC_MASK_BAD_ACCESS | EXC_MASK_BAD_INSTRUCTION | EXC_MASK_ARITHMETIC,
        MACH_PORT_NULL,
        EXCEPTION_STATE_IDENTITY,
        MACHINE_THREAD_STATE);

    if (kret != KERN_SUCCESS) {
        debugBelch("Could not disable CrashReporter. Mach error code %d\n", (int)kret);
    }

    sig_t oldSIGSEGV;
    sig_t oldSIGBUS;
    oldSIGSEGV = signal(SIGSEGV, segvHandler);
    oldSIGBUS = signal(SIGBUS, segvHandler);

    int compress = strstr(fileName, ".gz") != 0 ? 1 : 0;
    xmlTextWriterPtr writer = xmlNewTextWriterFilename(fileName, compress);
    xmlTextWriterSetIndent(writer, 1);
    xmlTextWriterSetIndentString(writer, (const xmlChar *)" ");

    xmlTextWriterStartDocument(writer, NULL, "utf-8", NULL);
    xmlTextWriterStartElement(writer, (const xmlChar *)"heap");

    nat g;
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        xmlTextWriterStartElement(writer, (const xmlChar *)"generation");
        xmlTextWriterWriteFormatAttribute(writer, (const xmlChar *)"number", "%d", g);
        printHeapChunkXml(writer, filter, context, generations[g].blocks);
        printHeapChunkXml(writer, filter, context, generations[g].large_objects);
        xmlTextWriterEndElement(writer); // </generation>
    }

    xmlTextWriterEndElement(writer); // </heap>
    xmlTextWriterEndDocument(writer);
    xmlFreeTextWriter(writer);

    signal(SIGSEGV, oldSIGSEGV);
    signal(SIGBUS, oldSIGBUS);
}

static rtsBool
filterNone (
  const void* ctx STG_UNUSED,
  const StgInfoTable* info STG_UNUSED,
  const StgClosure* obj STG_UNUSED)
{
    return rtsFalse;
}

static rtsBool
filterByTypeName (
  const void* ctx,
  const StgInfoTable* info,
  const StgClosure* obj)
{
    switch (info->type) {
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_2_0:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC: {
        const StgConInfoTable* con_info = get_con_itbl(obj);
        const char* con = GET_CON_DESC(con_info);
        if (strcasecmp(ctx, con) == 0) {
            return rtsFalse;
        }
    }

    default:
        return rtsTrue;
    }
}

extern void
printHeap (const char *fileName)
{
    printHeapWithFilter(fileName, filterNone, NULL);
}

extern void
printHeapByType (const char *fileName, const char *typeName)
{
    printHeapWithFilter(fileName, filterByTypeName, typeName);
}

#endif /* DEBUG */

