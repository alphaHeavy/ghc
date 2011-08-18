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

static void    printStdObjPayload( xmlTextWriterPtr writer, const StgClosure *obj );
#if 0 /* unused but might be useful sometime */
static rtsBool lookup_name   ( char *name, StgWord *result );
static void    enZcode       ( char *in, char *out );
#endif

extern void printClosure (xmlTextWriterPtr writer, const StgClosure *obj);

/* --------------------------------------------------------------------------
 * Printer
 * ------------------------------------------------------------------------*/

extern void
printObj (xmlTextWriterPtr writer, const StgClosure *obj)
{
    printClosure(writer, obj);
}

extern void
printPtr (const StgClosure *obj)
{
}

static void
writePointerAttribute(xmlTextWriterPtr writer, const char *name, StgPtr value)
{
    if (value != 0) {
        xmlTextWriterWriteFormatAttribute(writer, name, "%p", value);
    }
}

STATIC_INLINE void
printStdObjHdr (xmlTextWriterPtr writer, const StgClosure *obj, const char* tag)
{
    xmlTextWriterWriteAttribute(writer, "tag", tag);
    writePointerAttribute(writer, "info", ((StgPtr)obj->header.info));
#ifdef PROFILING
    xmlTextWriterWriteAttribute(writer, "label", obj->header.prof.ccs->cc->label);
#endif
}

static void
printInfoTablePayload (xmlTextWriterPtr writer, const StgInfoTable* info, const StgClosurePtr* payload)
{
    xmlTextWriterWriteFormatAttribute(writer, "ptrs", "%lu", info->layout.payload.ptrs);
    xmlTextWriterWriteFormatAttribute(writer, "nptrs", "%lu", info->layout.payload.nptrs);
    StgWord i;
    for (i = 0; i < info->layout.payload.ptrs; ++i) {
        if (payload[i] != 0) {
            xmlTextWriterStartElement(writer, "ptr");
            xmlTextWriterWriteFormatAttribute(writer, "addr", "%p", payload[i]);
            xmlTextWriterEndElement(writer); // </ptr>
        }
    }

    StgWord j;
    for (j = 0; j < info->layout.payload.nptrs; ++j) {
        if (payload[i+j] != 0) {
            xmlTextWriterStartElement(writer, "nptr");
            xmlTextWriterWriteFormatAttribute(writer, "val", "%p", payload[i+j]);
            xmlTextWriterEndElement(writer); // </ptr>
        }
    }
}

static void
printStdObjPayload (xmlTextWriterPtr writer, const StgClosure *obj)
{
    printInfoTablePayload(writer, get_itbl(obj), obj->payload);
}

static void
printThunkPayload (xmlTextWriterPtr writer, const StgThunk *obj)
{
    printInfoTablePayload(writer, get_itbl(obj), obj->payload);
}

static void
printThunkObject (xmlTextWriterPtr writer, const StgThunk *obj, const char* tag)
{
    printStdObjHdr(writer, (const StgClosure *)obj, tag);
    printThunkPayload(writer, obj);
}

extern void
printClosure (xmlTextWriterPtr writer, const StgClosure *obj)
{
    obj = UNTAG_CLOSURE(obj);

    const StgInfoTable* info;
    info = get_itbl(obj);

    xmlTextWriterStartElement(writer, closure_type_names[info->type]);
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
        xmlTextWriterWriteFormatAttribute(writer, "ptrs", "%lu", info->layout.payload.ptrs);
        xmlTextWriterWriteFormatAttribute(writer, "nptrs", "%lu", info->layout.payload.nptrs);

        const char* con = GET_CON_DESC(con_info);
        if (con != 0 && strlen(con) > 0) {
            xmlTextWriterWriteAttribute(writer, "con", GET_CON_DESC(con_info));
        }

        for (i = 0; i < info->layout.payload.ptrs; ++i) {
            if (obj->payload[i] != 0) {
                if (obj->payload[i] != 0) {
                    xmlTextWriterStartElement(writer, "ptr");
                    xmlTextWriterWriteFormatAttribute(writer, "addr", "%p", obj->payload[i]);
                    xmlTextWriterEndElement(writer); // </ptr>
                }
            }
        }
        for (j = 0; j < info->layout.payload.nptrs; ++j) {
            if (obj->payload[i+j] != 0) {
                if (obj->payload[i+j] != 0) {
                    xmlTextWriterStartElement(writer, "nptr");
                    xmlTextWriterWriteFormatAttribute(writer, "val", "%p", obj->payload[i+j]);
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
        xmlTextWriterWriteFormatAttribute(writer, "arity", "%d", itbl_to_fun_itbl(info)->f.arity);
        writePointerAttribute(writer, "info", ((StgPtr)obj->header.info));
#ifdef PROFILING
        xmlTextWriterWriteAttribute(writer, "label", obj->header.prof.ccs->cc->label);
#endif
        printStdObjPayload(writer, obj);
        break;

    case PRIM:
        writePointerAttribute(writer, "info", ((StgPtr)obj->header.info));
        printStdObjPayload(writer, obj);
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
        printThunkObject(writer, (const StgThunk *)obj, GET_PROF_DESC(info));
#else
        printThunkObject(writer, (const StgThunk *)obj, "THUNK");
#endif
        break;

    case THUNK_SELECTOR:
        printStdObjHdr(writer, obj, "THUNK_SELECTOR");
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
        writePointerAttribute(writer, "fun", (StgPtr)ap->fun);
        xmlTextWriterWriteFormatAttribute(writer, "args", "%lu", ap->n_args);
        for (i = 0; i < ap->n_args; ++i) {
            if (ap->payload[i] != 0) {
                xmlTextWriterStartElement(writer, "ptr");
                xmlTextWriterWriteFormatAttribute(writer, "addr", "%p", ((P_)ap->payload[i]));
                xmlTextWriterEndElement(writer); // </ptr>
            }
        }
        break;
    }

    case PAP: {
        const StgPAP* pap = (const StgPAP*)obj;
        StgWord i;
        xmlTextWriterWriteFormatAttribute(writer, "arity", "%d", pap->arity);
        writePointerAttribute(writer, "fun", (StgPtr)pap->fun);
        xmlTextWriterWriteFormatAttribute(writer, "args", "%lu", pap->n_args);
        for (i = 0; i < pap->n_args; ++i) {
            if (pap->payload[i] != 0) {
                xmlTextWriterStartElement(writer, "ptr");
                xmlTextWriterWriteFormatAttribute(writer, "addr", "%p", ((P_)pap->payload[i]));
                xmlTextWriterEndElement(writer); // </ptr>
            }
        }
        break;
    }

    case AP_STACK: {
        const StgAP_STACK* ap = (const StgAP_STACK*)obj;
        StgWord i;
        writePointerAttribute(writer, "fun", (StgPtr)ap->fun);
        xmlTextWriterWriteFormatAttribute(writer, "size", "%lu", ap->size);
        for (i = 0; i < ap->size; ++i) {
            if (ap->payload[i] != 0) {
                xmlTextWriterStartElement(writer, "ptr");
                xmlTextWriterWriteFormatAttribute(writer, "addr", "%p", ((P_)ap->payload[i]));
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
        xmlTextWriterWriteFormatAttribute(writer, "bytes", "%u", (unsigned int)arr->bytes);
        xmlTextWriterWriteBase64(writer, (const char *)arr->payload, 0, arr->bytes);
        break;
    }

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN: {
        const StgMutArrPtrs* arr = (const StgMutArrPtrs *)obj;
        xmlTextWriterWriteFormatAttribute(writer, "size", "%lu", (lnat)arr->ptrs);
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
        xmlTextWriterWriteFormatAttribute(writer, "id", "%lu", (lnat)tso->id);
        break;
    }

    case STACK: {
        const StgStack* stack = (const StgStack*)obj;
        xmlTextWriterWriteFormatAttribute(writer, "size", "%lu", (lnat)stack->stack_size);
        break;
    }

    default:
        // no additional details to add
        break;
    }

    xmlTextWriterEndElement(writer); // </object>
}

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

static jmp_buf jmpbuf;
static void
segvHandler (int signo)
{
    debugBelch("<<%s>>\n", strsignal(signo));
    siglongjmp(jmpbuf, 1);
}

static void
printHeapChunk (xmlTextWriterPtr writer, const bdescr *bd)
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

                StgPtr end = q + closure_sizeW((const StgClosure*)q);
                if (end >= bd->free) {
                    // debugBelch("%p found at %p, closure?", p, q);
                    continue;
                }

                printClosure(writer, (const StgClosure *)q);
            }
        }
    }
}

void
printHeap (const char *fileName)
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

    xmlTextWriterPtr writer = xmlNewTextWriterFilename(fileName, 1);
    xmlTextWriterSetIndent(writer, 1);
    xmlTextWriterSetIndentString(writer, " ");

    xmlTextWriterStartDocument(writer, NULL, "utf-8", NULL);
    xmlTextWriterStartElement(writer, "heap");

    nat g;
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
        xmlTextWriterStartElement(writer, "generation");
        xmlTextWriterWriteFormatAttribute(writer, "number", "%d", g);
        printHeapChunk(writer, generations[g].blocks);
        printHeapChunk(writer, generations[g].large_objects);
        xmlTextWriterEndElement(writer); // </generation>
    }

    xmlTextWriterEndElement(writer); // </heap>
    xmlTextWriterEndDocument(writer);
    xmlFreeTextWriter(writer);

    signal(SIGSEGV, oldSIGSEGV);
    signal(SIGBUS, oldSIGBUS);
}

const char *what_next_strs[] = {
  [0]               = "(unknown)",
  [ThreadRunGHC]    = "ThreadRunGHC",
  [ThreadInterpret] = "ThreadInterpret",
  [ThreadKilled]    = "ThreadKilled",
  [ThreadComplete]  = "ThreadComplete"
};

#endif /* DEBUG */

/* -----------------------------------------------------------------------------
   Closure types

   NOTE: must be kept in sync with the closure types in includes/ClosureTypes.h
   -------------------------------------------------------------------------- */

const char *closure_type_names[] = {
 [INVALID_OBJECT]        = "INVALID_OBJECT",
 [CONSTR]                = "CONSTR",
 [CONSTR_1_0]            = "CONSTR_1_0",
 [CONSTR_0_1]            = "CONSTR_0_1",
 [CONSTR_2_0]            = "CONSTR_2_0",
 [CONSTR_1_1]            = "CONSTR_1_1",
 [CONSTR_0_2]            = "CONSTR_0_2",
 [CONSTR_STATIC]         = "CONSTR_STATIC",
 [CONSTR_NOCAF_STATIC]   = "CONSTR_NOCAF_STATIC",
 [FUN]                   = "FUN",
 [FUN_1_0]               = "FUN_1_0",
 [FUN_0_1]               = "FUN_0_1",
 [FUN_2_0]               = "FUN_2_0",
 [FUN_1_1]               = "FUN_1_1",
 [FUN_0_2]               = "FUN_0_2",
 [FUN_STATIC]            = "FUN_STATIC",
 [THUNK]                 = "THUNK",
 [THUNK_1_0]             = "THUNK_1_0",
 [THUNK_0_1]             = "THUNK_0_1",
 [THUNK_2_0]             = "THUNK_2_0",
 [THUNK_1_1]             = "THUNK_1_1",
 [THUNK_0_2]             = "THUNK_0_2",
 [THUNK_STATIC]          = "THUNK_STATIC",
 [THUNK_SELECTOR]        = "THUNK_SELECTOR",
 [BCO]                   = "BCO",
 [AP]                    = "AP",
 [PAP]                   = "PAP",
 [AP_STACK]              = "AP_STACK",
 [IND]                   = "IND",
 [IND_PERM]              = "IND_PERM",
 [IND_STATIC]            = "IND_STATIC",
 [RET_BCO]               = "RET_BCO",
 [RET_SMALL]             = "RET_SMALL",
 [RET_BIG]               = "RET_BIG",
 [RET_DYN]               = "RET_DYN",
 [RET_FUN]               = "RET_FUN",
 [UPDATE_FRAME]          = "UPDATE_FRAME",
 [CATCH_FRAME]           = "CATCH_FRAME",
 [UNDERFLOW_FRAME]       = "UNDERFLOW_FRAME",
 [STOP_FRAME]            = "STOP_FRAME",
 [BLACKHOLE]             = "BLACKHOLE",
 [BLOCKING_QUEUE]        = "BLOCKING_QUEUE",
 [MVAR_CLEAN]            = "MVAR_CLEAN",
 [MVAR_DIRTY]            = "MVAR_DIRTY",
 [ARR_WORDS]             = "ARR_WORDS",
 [MUT_ARR_PTRS_CLEAN]    = "MUT_ARR_PTRS_CLEAN",
 [MUT_ARR_PTRS_DIRTY]    = "MUT_ARR_PTRS_DIRTY",
 [MUT_ARR_PTRS_FROZEN0]  = "MUT_ARR_PTRS_FROZEN0",
 [MUT_ARR_PTRS_FROZEN]   = "MUT_ARR_PTRS_FROZEN",
 [MUT_VAR_CLEAN]         = "MUT_VAR_CLEAN",
 [MUT_VAR_DIRTY]         = "MUT_VAR_DIRTY",
 [WEAK]                  = "WEAK",
 [PRIM]                  = "PRIM",
 [MUT_PRIM]              = "MUT_PRIM",
 [TSO]                   = "TSO",
 [STACK]                 = "STACK",
 [TREC_CHUNK]            = "TREC_CHUNK",
 [ATOMICALLY_FRAME]      = "ATOMICALLY_FRAME",
 [CATCH_RETRY_FRAME]     = "CATCH_RETRY_FRAME",
 [CATCH_STM_FRAME]       = "CATCH_STM_FRAME",
 [WHITEHOLE]             = "WHITEHOLE"
};

const char *
info_type (const StgClosure *closure) {
  return closure_type_names[get_itbl(closure)->type];
}

const char *
info_type_by_ip (const StgInfoTable *ip) {
  return closure_type_names[ip->type];
}

void
info_hdr_type (const StgClosure *closure, char *res) {
  strcpy(res, closure_type_names[get_itbl(closure)->type]);
}

