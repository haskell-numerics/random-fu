* Changes in 0.3.0.11: Revert 0.3.0.10 changes (which accidentally removed the Random.Source.PureMT module and added an overlapping instance).

* Changes in 0.3.0.8: Add MonadRandom instance for MWC generator and RWS transformer.

* Changes in 0.3.0.6: Fixed overzealous fix in 0.3.0.5.  The people responsible for sacking the people who have been sacked, etc., have been sacked.

* Changes in 0.3.0.5: Renamed some internal modules and accidentally some external ones too.  Whoops.  Please don't use this version, it will only end in tears.

* Changes in 0.3.0.4: Fixed a typo that broke building with MTL-1

* Changes in 0.3.0.3: Fixes for GHC's deprecation of Foreign.unsafePerformIO

* Changes in 0.3.0.2: Fixes for GHC 7.2.*'s crazy Template Haskell changes.
