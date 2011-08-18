/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prototypes for functions in PrinterXml.c
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRINTERXML_H
#define PRINTERXML_H

#include "BeginPrivate.h"

#ifdef DEBUG
extern void          printHeap       ( const char *fileName );
extern void          printHeapByType ( const char *fileName, const char *typeName );
#endif

#include "EndPrivate.h"

#endif /* PRINTERXML_H */

