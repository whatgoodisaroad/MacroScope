FindRefinements:
	ghc -O2 -rtsopts -prof -auto-all FindRefinements.hs -i../src -i../lib/CPP-Parser -o FindRefinements
	#ghc FindRefinements.hs -i../src -i../lib/CPP-Parser -o FindRefinements
	mv FindRefinements ~/macrosurvey/

ExtractForest:
	ghc ExtractForest.hs -i../src -i../lib/CPP-Parser
	mv ExtractForest ~/macrosurvey/

clean:
	rm *.o *.hi