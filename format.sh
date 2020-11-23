ormolu -p -m inplace \
       $(find src \
	      -type f \
	      -name "*.hs" \
	      ! -path "src/Settings.hs" \
       )
