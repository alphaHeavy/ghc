/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prototypes for functions in Printer.c
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRINTER_H
#define PRINTER_H

#include "BeginPrivate.h"

extern void          printPtr        ( StgPtr p );
extern void          printObj        ( StgClosure *obj );

extern const char *  closure_type_names[];

void                 info_hdr_type   ( StgClosure *closure, char *res );
const char *         info_type       ( StgClosure *closure );
const char *         info_type_by_ip ( StgInfoTable *ip );

#ifdef DEBUG
extern void          prettyPrintClosure (StgClosure *obj);
extern void          printClosure    ( StgClosure *obj );
extern StgPtr        printStackObj   ( StgPtr sp );
extern void          printStackChunk ( StgPtr sp, StgPtr spLim );
extern void          printTSO        ( StgTSO *tso );
extern void          printHeap       ( void );

extern void          loadSymbols     ( char *name );

extern const char *  lookupGHCName   ( void *addr );
extern const char *  what_next_strs[];
#endif

#include "EndPrivate.h"

#endif /* PRINTER_H */

