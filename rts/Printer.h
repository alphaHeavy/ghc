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

extern const char *  closure_type_names[];

void                 info_hdr_type   ( const StgClosure *closure, char *res );
const char *         info_type       ( const StgClosure *closure );
const char *         info_type_by_ip ( const StgInfoTable *ip );

#ifdef DEBUG
extern void          printHeap       ( const char *fileName );

extern void          loadSymbols     ( const char *name );

extern const char *  lookupGHCName   ( const void *addr );
extern const char *  what_next_strs[];
#endif

#include "EndPrivate.h"

#endif /* PRINTER_H */

