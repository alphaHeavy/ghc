{-# LANGUAGE CPP #-}

-- -----------------------------------------------------------------------------
-- | Generates meta information about the program useful for debugging and profiling
--

module LlvmMeta ( cmmMetaLlvmGens, cmmDebugLlvmGens ) where

import Llvm
import Llvm.Types      ( LMMetaInt(..) )

import LlvmCodeGen.Base
import LlvmCodeGen.Ppr

import CLabel
import Module
import DynFlags
import FastString

import Config          ( cProjectName, cProjectVersion )
import Name            ( nameOccName )
import Literal         ( Literal(..) )
import OccName         ( occNameFS )
import Var             ( Var, varName )
import OldCmm
import Outputable
import CoreSyn
import Platform
import SrcLoc          (srcSpanFile,
                        srcSpanStartLine, srcSpanStartCol,
                        srcSpanEndLine, srcSpanEndCol)
import MonadUtils      ( MonadIO(..) )

import System.Directory(getCurrentDirectory)
import Control.Monad   (forM)
import Data.Bits       ((.|.), (.&.), shiftL)
import Data.List       (nub, maximumBy)
import Data.Maybe      (fromMaybe, mapMaybe, catMaybes)
import Data.Map as Map (Map, fromList, assocs, lookup, elems)
import Data.Set as Set (Set, fromList, member)
import Data.Function   (on)
import Data.Char       (ord, isAscii, isPrint, intToDigit)
import Data.Word       (Word16)

#define EVENTLOG_CONSTANTS_ONLY
#include "../../includes/rts/EventLogFormat.h"

-- Constants

lLVMDebugVersion, dW_TAG_compile_unit, dW_TAG_subroutine_type :: Integer
dW_TAG_file_type, dW_TAG_subprogram, dW_TAG_lexical_block :: Integer
dW_TAG_structure_type, dW_TAG_pointer_type, dW_TAG_typedef :: Integer
dW_TAG_base_type, dW_TAG_arg_variable :: Integer
lLVMDebugVersion = 0x90000

dW_TAG_arg_variable    = 257 + lLVMDebugVersion
dW_TAG_base_type       = 36  + lLVMDebugVersion
dW_TAG_compile_unit    = 17  + lLVMDebugVersion
dW_TAG_file_type       = 41  + lLVMDebugVersion
dW_TAG_lexical_block   = 11  + lLVMDebugVersion
dW_TAG_pointer_type    = 15  + lLVMDebugVersion
dW_TAG_structure_type  = 19  + lLVMDebugVersion
dW_TAG_subprogram      = 46  + lLVMDebugVersion
dW_TAG_subroutine_type = 32  + lLVMDebugVersion
dW_TAG_typedef         = 22  + lLVMDebugVersion

dW_LANG_Haskell :: Integer
dW_LANG_Haskell  = 0x8042 -- Chosen arbitrarily

dI_FLAG_private, dI_FLAG_protected, dI_FLAG_forward_decl :: Integer
dI_FLAG_apple_block, dI_FLAG_block_byref_struct :: Integer
dI_FLAG_virtual, dI_FLAG_artificial, dI_FLAG_explicit :: Integer
dI_FLAG_prototyped, dI_FLAG_objc_class_complete :: Integer

dI_FLAG_private             = 0x001
dI_FLAG_protected           = 0x002
dI_FLAG_forward_decl        = 0x004
dI_FLAG_apple_block         = 0x008
dI_FLAG_block_byref_struct  = 0x010
dI_FLAG_virtual             = 0x020
dI_FLAG_artificial          = 0x040
dI_FLAG_explicit            = 0x080
dI_FLAG_prototyped          = 0x100
dI_FLAG_objc_class_complete = 0x200

pprMeta :: LMMetaInt -> LlvmStatic -> SDoc
pprMeta n val = pprLlvmData ([LMGlobal (LMGlobalMeta n) (Just val)], [])

-- | Prints the given metadata to the output handle
renderMeta :: LlvmStatic -> LlvmM LMMetaInt
renderMeta lmm = do
    metaId <- getMetaUniqueId
    renderLlvm $ pprMeta metaId lmm
    return metaId

cmmMetaLlvmGens :: DynFlags -> ModLocation -> TickMap -> [RawCmmDecl] -> LlvmM ()
cmmMetaLlvmGens dflags mod_loc tiMap cmm = do

  -- We need to be able to find ticks by ID
  let idLabelMap :: Map Int CLabel
      idLabelMap = Map.fromList $ mapMaybe ilm_entry $ assocs tiMap
      ilm_entry (l, TickMapEntry { timInstr = Just i })
                                    = Just (i, l)
      ilm_entry _                   = Nothing

  -- Allocate IDs. The instrumentation numbers will have been used for
  -- line annotations, so we make new IDs start right after them.
  setMetaSeed (LMMetaInt . fromMaybe 0 $ maximum (Nothing:map timInstr (elems tiMap)))
  let freshId = getMetaUniqueId

  -- Emit compile unit information.
  srcPath <- liftIO $ getCurrentDirectory
  let srcFile  = fromMaybe "" (ml_hs_file mod_loc)
      producerName = cProjectName ++ " " ++ cProjectVersion

  enumTypesId <- freshId
  retainedTypesId <- freshId
  subprogramsId <- freshId
  globalsId <- freshId

  unitId <- renderMeta $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_compile_unit)
    , LMStaticLit (LMNullLit LMMetaType)         -- "unused"
    , LMStaticLit (mkI32 dW_LANG_Haskell)        -- DWARF language identifier
    , LMMetaString (fsLit srcFile)               -- Source code name
    , LMMetaString (fsLit srcPath)               -- Compilation base directory
    , LMMetaString (fsLit producerName)          -- Producer
    , LMStaticLit (mkI1 True)                    -- Main compilation unit?
                                                 -- Not setting this causes LLVM to not generate anything at all!
    , LMStaticLit (mkI1 $ optLevel dflags > 0)   -- Optimized?
    , LMMetaString (fsLit "")                    -- Flags (?)
    , LMStaticLit (mkI32 0)                      -- Runtime version (?)
    , LMMetaRef enumTypesId                      -- List of enums types
    , LMMetaRef retainedTypesId                  -- List of retained types
    , LMMetaRef subprogramsId                    -- List of subprograms
    , LMMetaRef globalsId                        -- List of global variables
    ]

  -- Subprogram type we use: void (*)(StgBaseReg*)
  srtypeArgsId <- freshId
  srtypeId <- renderMeta $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_subroutine_type)
    , LMMetaRef unitId                           -- Context
    , LMMetaString (fsLit "StgCall")             -- Name (anonymous)
    , LMStaticLit (LMNullLit LMMetaType)         -- File where defined (null)
    , LMStaticLit (mkI32 0)                      -- Line where defined
    , LMStaticLit (mkI64 64)                     -- Size in bits
    , LMStaticLit (mkI64 32)                     -- Alignment in bits
    , LMStaticLit (mkI64 0)                      -- Offset in bits
    , LMStaticLit (mkI32 0)                      -- Flags in bits
    , LMStaticLit (LMNullLit LMMetaType)         -- Type derived from
    , LMMetaRef srtypeArgsId                     -- Type list (just void)
    , LMStaticLit (mkI32 0)                      -- Runtime languages (?)
    ]

  -- Emit metadata for all files
  let files = nub $ map (srcSpanFile . sourceSpan) $
              concatMap (filter isSourceTick . timTicks) $
              Map.elems tiMap
  fileMap <- fmap Map.fromList $ forM files $ \file -> do

    -- We somewhat sneakily use the information present in
    -- SrcSpan here.

    -- TODO: Note that right this might come from other modules via
    -- inlining, so we might get bad mixing of base paths here.
    fileId <- freshId
    emitFileMeta fileId unitId (unpackFS file)
    return (file, fileId)

  -- Unless we already have one, emit a "default" file metadata for
  -- the compilation unit. This will be used to annotate all
  -- procedures which have otherwise no associated debug data (so they
  -- won't simply get discarded)
  let unitFile = fromMaybe "** no source file **" (ml_hs_file mod_loc)
  defaultFileId <- case Map.lookup (fsLit unitFile) fileMap of
    Just fileId -> return fileId
    Nothing     -> do
      fileId <- freshId
      emitFileMeta fileId unitId unitFile
      return fileId

  primitives@StgPrimitives{stgRegTableId = baseRegId} <- emitStgPrimitivesMeta unitId defaultFileId

  srtypeArgsArrayId <- freshId
  renderLlvm $ pprMeta srtypeArgsId $ LMMeta [LMMetaRef srtypeArgsArrayId]
  renderLlvm $ pprMeta srtypeArgsArrayId $ LMMeta [LMMetaRef baseRegId]

  -- Lookup of procedure Cmm data
  let procMap = Map.fromList [ (l, p) | p@(CmmProc _ l _) <- cmm ]

  -- Emit metadata for files and procedures
  procIds <- forM (assocs tiMap) $ \(lbl, tim) -> do

    -- Decide what source code to associate with procedure
    let procTick = findGoodSourceTick lbl unitFile tiMap idLabelMap
        srcFileLookup = flip Map.lookup fileMap . srcSpanFile . sourceSpan
        fileId = fromMaybe defaultFileId (procTick >>= srcFileLookup)
        loc@(line, col) = case fmap sourceSpan procTick of
          Just span -> (srcSpanStartLine span, srcSpanStartCol span)
          _         -> (1, 0)

    -- Find procedure in Cmm data
    case Map.lookup lbl procMap of
      Just (CmmProc infos _ (ListGraph blocks)) | not (null blocks) -> do

        -- Generate metadata for procedure
        let entryLabel = case infos of
              Nothing               -> lbl
              Just (Statics lbl' _) -> lbl'
            contextId = fromMaybe unitId (fmap fromIntegral (timParent tim))
        procId <- freshId
        emitProcMeta procId contextId srtypeId entryLabel fileId loc dflags

        -- Generate source annotation using the given ID (this is used to
        -- reference it from LLVM code). This information has little
        -- direct use for actual debugging, but prevents LLVM from just
        -- throwing the above debugging information away.
        --
        -- Note that this would end up generating duplicated metadata if
        -- instrumentation IDs weren't unique per procedure!
        case timInstr tim of
          Just i -> do
            emitArgumentsMeta primitives (fromIntegral i) procId fileId loc

            blockId <- freshId
            renderLlvm $ pprMeta blockId $ LMMeta $
              [ LMStaticLit (mkI32 $ dW_TAG_lexical_block)
              , LMMetaRef procId
              , LMStaticLit (mkI32 $ fromIntegral line) -- Source line
              , LMStaticLit (mkI32 $ fromIntegral col)  -- Source column
              , LMMetaRef fileId                        -- File context
              , LMStaticLit (mkI32 0)                   -- Template parameter index
              ]
            renderLlvm $ pprMeta (LMMetaInt i) $ LMMeta $
              [ LMStaticLit (mkI32 $ fromIntegral line) -- Source line
              , LMStaticLit (mkI32 $ fromIntegral col)  -- Source column
              , LMMetaRef blockId                       -- Block context
              , LMStaticLit (LMNullLit LMMetaType)      -- Inlined from location
              ]

          Nothing -> return ()

        return $ Just procId

      -- Without CMM source data for the procedure, we are not able to
      -- generate valid DWARF for it
      _otherwise -> return Nothing

  -- Generate a list of all generate subprogram metadata structures
  zeroId <- freshId
  zeroArrayId <- freshId
  let refs xs | null xs'  = LMMeta [LMMetaRef zeroArrayId]
              | otherwise = LMMeta (map LMMetaRef xs')
        where xs' = catMaybes xs

      renderRetainer retainId retainRefs = do
        arrayId <- freshId
        renderLlvm $ pprMeta retainId $ LMMeta [LMMetaRef arrayId]
        renderLlvm $ pprMeta arrayId $ refs retainRefs 

  -- retained debug metadata is a reference to an array
  renderLlvm $ pprMeta zeroArrayId $ LMMeta [LMMetaRef zeroId]
  renderLlvm $ pprMeta zeroId $ LMMeta [LMStaticLit (mkI32 0)]

  renderRetainer enumTypesId []
  renderRetainer retainedTypesId [Just srtypeId]
  renderRetainer subprogramsId procIds
  renderRetainer globalsId []

  -- the DWARF printer prefers all debug metadata
  -- to be referenced from a single global metadata
  renderLlvm $ pprLlvmData
    ([LMGlobal (LMNamedMeta (fsLit "llvm.dbg.cu"))
               (Just (LMMetaRefs [unitId]))], [])

  return ()

emitFileMeta :: LMMetaInt -> LMMetaInt -> FilePath -> LlvmM ()
emitFileMeta fileId unitId filePath = do
  srcPath <- liftIO $ getCurrentDirectory
  renderLlvm $ pprMeta fileId $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_file_type)
    , LMMetaString (fsLit filePath)              -- Source file name
    , LMMetaString (fsLit srcPath)               -- Source file directory
    , LMMetaRef unitId                           -- Reference to compile unit
    ]

emitProcMeta :: LMMetaInt -> LMMetaInt -> LMMetaInt -> CLabel -> LMMetaInt
             -> (Int, Int) -> DynFlags -> LlvmM ()
emitProcMeta procId contextId srtypeId entryLabel fileId (line, _) dflags = do
  -- it seems like LLVM 3.0 (likely 2.x as well) ignores the procedureName
  -- procedureName <- strProcedureName_llvm entryLabel
  linkageName <- strCLabel_llvm entryLabel
  displayName <- strDisplayName_llvm entryLabel

  let funRef = LMGlobalVar linkageName (LMPointer llvmFunTy) Internal Nothing Nothing True
      local = not . externallyVisibleCLabel $ entryLabel
      procedureName = displayName

  renderLlvm $ pprMeta procId $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_subprogram)
    , LMStaticLit (mkI32 0)                      -- "Unused"
    , LMMetaRef contextId                        -- Reference to context descriptor
    , LMMetaString procedureName                 -- Procedure name
    , LMMetaString displayName                   -- Display name
    , LMMetaString linkageName                   -- MIPS name
    , LMMetaRef fileId                           -- Reference to file
    , LMStaticLit (mkI32 $ fromIntegral line)    -- Line number
    , LMMetaRef srtypeId                         -- Type descriptor
    , LMStaticLit (mkI1 local)                   -- Local to compile unit
    , LMStaticLit (mkI1 True)                    -- Defined here (not "extern")
    , LMStaticLit (mkI32 0)                      -- Virtuality (none)
    , LMStaticLit (mkI32 0)                      --
    , LMStaticLit (LMNullLit LMMetaType)         --
    , LMStaticLit (mkI1 False)                   -- Artificial (?!)
    , LMStaticLit (mkI1 $ optLevel dflags > 0)   -- Optimized
    , LMStaticPointer funRef                     -- Function pointer
    ]

emitArgumentsMeta :: StgPrimitives -> LMMetaInt -> LMMetaInt -> LMMetaInt -> (Int, Int) -> LlvmM ()
emitArgumentsMeta primitives argId procId fileId loc = do
  emitRegMeta 1 "BaseReg" argId procId (stgRegTableId primitives) fileId loc
  -- int64_t* restrict sp,
  -- int64_t* restrict hp,
  emitRegMeta 4 "R1" argId procId (stgIntId primitives) fileId loc
  emitRegMeta 5 "R2" argId procId (stgIntId primitives) fileId loc
  emitRegMeta 6 "R3" argId procId (stgIntId primitives) fileId loc
  emitRegMeta 7 "R4" argId procId (stgIntId primitives) fileId loc
  emitRegMeta 8 "R5" argId procId (stgIntId primitives) fileId loc
  emitRegMeta 9 "R6" argId procId (stgIntId primitives) fileId loc
  --  int64_t* restrict spLim,
  emitRegMeta 11 "F1" argId procId (stgFloatId primitives) fileId loc
  emitRegMeta 12 "F2" argId procId (stgFloatId primitives) fileId loc
  emitRegMeta 13 "F3" argId procId (stgFloatId primitives) fileId loc
  emitRegMeta 14 "F4" argId procId (stgFloatId primitives) fileId loc

  emitRegMeta 15 "D1" argId procId (stgDoubleId primitives) fileId loc
  emitRegMeta 16 "D2" argId procId (stgDoubleId primitives) fileId loc

emitRegMeta :: Int -> String -> LMMetaInt -> LMMetaInt -> LMMetaInt -> LMMetaInt -> (Int, Int) -> LlvmM ()
emitRegMeta argNum argName argId procId typeId fileId (line, _) = do
  let argLine = (argNum `shiftL` 24) .|. (0xFFFFFF .&. line)
  renderLlvm $ pprMeta (fromIntegral (argNum * 1000000) + argId) $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_arg_variable)
    , LMMetaRef procId                           -- Context
    , LMMetaString (fsLit argName)               -- Name
    , LMMetaRef fileId                           -- Reference to file where defined
    , LMStaticLit (mkI32 $ fromIntegral argLine) -- 24 bit - Line number where defined
                                                 -- 8 bit - Argument number
    , LMMetaRef typeId                           -- Type descriptor
    , LMStaticLit (mkI32 0)                      -- flags
    ]

data StgPrimitives = StgPrimitives
  { stgRegTableId :: {-# UNPACK #-} !LMMetaInt
  , stgIntId      :: {-# UNPACK #-} !LMMetaInt
  , stgFloatId    :: {-# UNPACK #-} !LMMetaInt
  , stgDoubleId   :: {-# UNPACK #-} !LMMetaInt
  } 

emitStgPrimitivesMeta :: LMMetaInt -> LMMetaInt -> LlvmM StgPrimitives
emitStgPrimitivesMeta unitId defaultFileId = do
  baseRegId <- renderMeta $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_structure_type)
    , LMMetaRef unitId                           -- Reference to context
    , LMMetaString (fsLit "StgRegTable_")        -- Source code name
    -- for whatever reason LLVM discards forward declarations unless
    -- they have a file context and a line number greater than 0
    , LMMetaRef defaultFileId                    -- Reference to file where defined 1
    , LMStaticLit (mkI32 1)                      -- Line number where defined
    , LMStaticLit (mkI64 0)                      -- Size in bits 3
    , LMStaticLit (mkI64 0)                      -- Alignment in bits
    , LMStaticLit (mkI32 0)                      -- Offset in bits 5
    , LMStaticLit (mkI32 dI_FLAG_forward_decl)   -- Flags
    , LMStaticLit (mkI32 0)                      -- Reference to type derived from 7
    , LMStaticLit (LMNullLit LMMetaType)         -- Reference to array of member descriptors
    , LMStaticLit (mkI32 0)                      -- Runtime languages 9
    , LMStaticLit (mkI32 0)                      -- 
    ]

  baseRegPtrId <- renderMeta $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_pointer_type)
    , LMMetaRef unitId                           -- Reference to context
    , LMMetaString (fsLit "")                    -- Name (may be "" for anonymous types)
    , LMStaticLit (LMNullLit LMMetaType)         -- Reference to file where defined (may be NULL)
    , LMStaticLit (mkI32 0)                      -- Line number where defined
    , LMStaticLit (mkI64 64)                     -- Size in bits XXX: find pointer size
    , LMStaticLit (mkI64 64)                     -- Alignment in bits XXX: find pointer alignment
    , LMStaticLit (mkI64 0)                      -- Offset in bits
    , LMStaticLit (mkI32 0)                      -- Flags to encode attributes, e.g. private
    , LMMetaRef baseRegId                        -- Reference to type derived from
    ]

  let dW_ATE_signed        = 5
  intId <- renderMeta $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_base_type)
    , LMMetaRef unitId                           -- Reference to context
    , LMMetaString (fsLit "StgInt")              -- Source code name
    , LMStaticLit (LMNullLit LMMetaType)         -- Reference to file where defined
    , LMStaticLit (mkI32 0)                      -- Line number where defined
    , LMStaticLit (mkI64 64)                     -- Size in bits
    , LMStaticLit (mkI64 64)                     -- Alignment in bits
    , LMStaticLit (mkI64 0)                      -- Offset in bits
    , LMStaticLit (mkI32 0)                      -- Flags
    , LMStaticLit (mkI32 dW_ATE_signed)          -- DWARF type encoding
    ]

  let dW_ATE_float         = 4
  floatId <- renderMeta $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_base_type)
    , LMMetaRef unitId                           -- Reference to context
    , LMMetaString (fsLit "StgFloat")            -- Source code name
    , LMStaticLit (LMNullLit LMMetaType)         -- Reference to file where defined
    , LMStaticLit (mkI32 0)                      -- Line number where defined
    , LMStaticLit (mkI64 32)                     -- Size in bits
    , LMStaticLit (mkI64 32)                     -- Alignment in bits
    , LMStaticLit (mkI64 0)                      -- Offset in bits
    , LMStaticLit (mkI32 0)                      -- Flags
    , LMStaticLit (mkI32 dW_ATE_float)           -- DWARF type encoding
    ]

  doubleId <- renderMeta $ LMMeta
    [ LMStaticLit (mkI32 dW_TAG_base_type)
    , LMMetaRef unitId                           -- Reference to context
    , LMMetaString (fsLit "StgDouble")           -- Source code name
    , LMStaticLit (LMNullLit LMMetaType)         -- Reference to file where defined
    , LMStaticLit (mkI32 0)                      -- Line number where defined
    , LMStaticLit (mkI64 32)                     -- Size in bits
    , LMStaticLit (mkI64 32)                     -- Alignment in bits
    , LMStaticLit (mkI64 0)                      -- Offset in bits
    , LMStaticLit (mkI32 0)                      -- Flags
    , LMStaticLit (mkI32 dW_ATE_float)           -- DWARF type encoding
    ]

  return $! StgPrimitives baseRegPtrId intId floatId doubleId

-- | Find a "good" tick we could associate the procedure with in the
-- DWARF debugging data. We do this by looking for source ticks at the
-- given procedure as well as the context that it was created from
-- ("parent").
--
-- As this might often give us a whole list of ticks to choose from,
-- we arbitrarily select the biggest source span - preferably from the
-- source file we are currently compiling - and hope that it
-- corresponds to the most useful location in the code. All nothing
-- but guesswork, obviously, but this is meant to be more or lesser
-- filler data anyway.
findGoodSourceTick :: CLabel -> FilePath -> TickMap -> Map Int CLabel -> Maybe (Tickish ())
findGoodSourceTick lbl unit tiMap idLabelMap
  | null ticks = Nothing
  | otherwise  = Just $ maximumBy (compare `on` rangeRating) ticks
  where
    unitFS = mkFastString unit
    ticks = findSourceTis lbl
    rangeRating (SourceNote span _) =
      (srcSpanFile span == unitFS,
       srcSpanEndLine span - srcSpanStartLine span,
       srcSpanEndCol span - srcSpanStartCol span)
    rangeRating _non_source_note = error "rangeRating"
    findSourceTis :: CLabel -> [Tickish ()]
    findSourceTis l = case Map.lookup l tiMap of
      Just tim
        | stis <- filter isSourceTick (timTicks tim), not (null stis)
        -> stis
        | Just p <- timParent tim, Just l' <- Map.lookup p idLabelMap
        -> findSourceTis l'
      _ -> []

isSourceTick :: Tickish () -> Bool
isSourceTick SourceNote {} = True
isSourceTick _             = False

-- | Intelligent constructor, deriving the type of the structure
-- automatically.
mkStaticStruct :: [LlvmStatic] -> LlvmStatic
mkStaticStruct elems = LMStaticStruc elems typ
  where typ = LMStruct $ map getStatType elems

-- | Automatically calculates the correct size of the string
mkStaticString :: String -> LlvmStatic
-- It seems strings aren't really supported in the LLVM back-end right
-- now - probably mainly because LLVM outputs Haskell strings as
-- arrays. So this is a bit bizarre: We need to escape the string
-- first, as well as account for the fact that the backend adds a
-- "\\00" that we actually neither want nor need. I am a bit unsure
-- though what the "right thing to do" is here, therefore I'll just
-- hack around it for the moment. Also: This is inefficient. -- PMW
--
-- Note: The terminating '\0' is now required by the RTS so it can
-- treat pointers into the event log as null-terminated C strings.
mkStaticString str = LMStaticStr (mkFastString (concatMap esc str)) typ
  where typ = LMArray (length str + 1) i8
        esc '\\'   = "\\\\"
        esc '\"'   = "\\22"
        esc '\n'   = "\\0a"
        esc c | isAscii c && isPrint c = [c]
        esc c      = ['\\', intToDigit (ord c `div` 16), intToDigit (ord c `mod` 16)]

-- | Collapses a tree of structures into a single structure, which has the same
-- data layout, but is quicker to produce
flattenStruct :: LlvmStatic -> LlvmStatic
flattenStruct start = mkStaticStruct $ go start []
  where go (LMStaticStruc elems _) xs = foldr go xs elems
        go other                   xs = other:xs

-- | Outputs a 16 bit word in big endian. This is required for all
-- values that the RTS will just copy into the event log later
mkLit16BE :: Int -> LlvmStatic
 -- This is obviously not portable. I suppose you'd have to either
 -- generate a structure or detect our current alignment.
mkLit16BE n = LMStaticLit $ mkI16 $ fromIntegral $ (b1 + 256 * b2)
  where (b1, b2) = (fromIntegral n :: Word16) `divMod` 256

-- | Packs the given static value into a (variable-length) event-log
-- packet.
mkEvent :: Int -> LlvmStatic -> LlvmStatic
mkEvent id cts
  = mkStaticStruct [ LMStaticLit $ mkI8 $ fromIntegral id
                   , LMStaticLit $ mkI16 $ fromIntegral size
                   , cts]
  where size = (llvmWidthInBits (getStatType cts) + 7) `div` 8

mkModuleEvent :: ModLocation -> LlvmStatic
mkModuleEvent mod_loc
  = mkEvent EVENT_DEBUG_MODULE $ mkStaticStruct
      [ mkStaticString $ case ml_hs_file mod_loc of
           Just file -> file
           _         -> "??"
      ]

mkProcedureEvent :: Platform -> TickMapEntry -> CLabel -> LlvmStatic
mkProcedureEvent platform tim lbl
  = mkEvent EVENT_DEBUG_PROCEDURE $ mkStaticStruct
      [ mkLit16BE $ fromMaybe none $ timInstr tim
      , mkLit16BE $ fromMaybe none $ timParent tim
      , mkStaticString $ showSDocC  $ pprCLabel platform lbl
      ]
  where none = 0xffff
        showSDocC = flip renderWithStyle (mkCodeStyle CStyle)

mkAnnotEvent :: Set Var -> Tickish () -> [LlvmStatic]
mkAnnotEvent _ (SourceNote ss names)
  = [mkEvent EVENT_DEBUG_SOURCE $ mkStaticStruct
      [ mkLit16BE $ srcSpanStartLine ss
      , mkLit16BE $ srcSpanStartCol ss
      , mkLit16BE $ srcSpanEndLine ss
      , mkLit16BE $ srcSpanEndCol ss
      , mkStaticString $ unpackFS $ srcSpanFile ss
      , mkStaticString names
      ]]

mkAnnotEvent bnds (CoreNote lbl (ExprPtr core))
  = [mkEvent EVENT_DEBUG_CORE $ mkStaticStruct
      [ mkStaticString $ showSDoc $ ppr lbl
      , mkStaticString $ take 10000 $ showSDoc $ ppr $ stripCore bnds core
      ]]
mkAnnotEvent _ _ = []

mkEvents :: Platform -> Set Var ->
            ModLocation -> TickMap -> [RawCmmDecl] -> LlvmStatic
mkEvents platform bnds mod_loc tick_map cmm
  = mkStaticStruct $
      [ mkModuleEvent mod_loc ] ++
      concat [ mkProcedureEvent platform tim (proc_lbl i l)
               : concatMap (mkAnnotEvent bnds) (timTicks tim)
             | CmmProc i l _ <- cmm
             , Just tim <- [Map.lookup l tick_map]] ++
      [ LMStaticLit $ mkI8 0 ]
  where proc_lbl (Just (Statics info_lbl _)) _ = info_lbl
        proc_lbl _                           l = l

collectBinds :: Tickish () -> [Var]
collectBinds (CoreNote bnd _) = [bnd]
collectBinds _                = []

cmmDebugLlvmGens :: DynFlags -> ModLocation ->
                    TickMap -> [RawCmmDecl] -> LlvmM ()
cmmDebugLlvmGens dflags mod_loc tick_map cmm = do

  let collect tim = concatMap collectBinds $ timTicks tim
      binds =  Set.fromList $ concatMap collect $ elems tick_map

  let platform = targetPlatform dflags

  let events = flattenStruct $ mkEvents platform binds mod_loc tick_map cmm

  -- Names for symbol / section
  let debug_sym  = fsLit $ "__debug_ghc"
      sectPrefix = case platformOS platform of
        OSDarwin -> "__DWARF,"
        _        -> "."
      sectName   = Just $ fsLit (sectPrefix ++ "debug_ghc")
      lmDebugVar = LMGlobalVar debug_sym (getStatType events) Internal sectName Nothing False
      lmDebug    = LMGlobal lmDebugVar (Just events)

  renderLlvm $ pprLlvmData ([lmDebug], [])
  markUsedVar lmDebugVar

mkI8, mkI16, mkI32, mkI64 :: Integer -> LlvmLit
mkI8 n = LMIntLit n (LMInt 8)
mkI16 n = LMIntLit n (LMInt 16)
mkI32 n = LMIntLit n (LMInt 32)
mkI64 n = LMIntLit n (LMInt 64)
mkI1 :: Bool -> LlvmLit
mkI1 f = LMIntLit (if f then 1 else 0) (LMInt 1)

placeholder :: Var -> CoreExpr
placeholder = Lit . MachStr . occNameFS . nameOccName . varName -- for now

stripCore :: Set Var -> CoreExpr -> CoreExpr
stripCore bs (App e1 e2) = App (stripCore bs e1) (stripCore bs e2)
stripCore bs (Lam b e)
  | b `member` bs        = Lam b (placeholder b)
  | otherwise            = Lam b (stripCore bs e)
stripCore bs (Let es e)  = Let (stripLet bs es) (stripCore bs e)
stripCore bs (Tick _ e)  = stripCore bs e -- strip out
stripCore bs (Case e b t as)
  | b `member` bs        = Case (stripCore bs e) b t [(DEFAULT,[],placeholder b)]
  | otherwise            = Case (stripCore bs e) b t (map stripAlt as)
  where stripAlt (a, bn, e) = (a, bn, stripCore bs e)
stripCore bs (CoreSyn.Cast e _)  = stripCore bs e -- strip out
stripCore _  other       = other

stripLet :: Set Var -> CoreBind -> CoreBind
stripLet bs (NonRec b e)
  | b `member` bs        = NonRec b (placeholder b)
  | otherwise            = NonRec b (stripCore bs e)
stripLet bs (Rec ps)     = Rec (map f ps)
  where
    f (b, e)
      | b `member` bs    = (b, placeholder b)
      | otherwise        = (b, stripCore bs e)
