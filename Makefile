
code/gabow/gabow.svg : code/gabow/Main.hs
	cd code/gabow/ && nix-shell --command 'cabal run'

images/gabow.png : code/gabow/gabow.svg
	convert -density 300 $< $@

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.html" -exec rm {} \;
	find . -name "*.tex" -exec rm {} \;
	find . -name "*.pdf" -exec rm {} \;
