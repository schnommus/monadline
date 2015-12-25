monadline: MonadLine.hs Segments.hs PowerlineCharacters.hs Main.hs
	ghc -o monadline Segments.hs MonadLine.hs PowerlineCharacters.hs Main.hs

clean:
	rm *.o *.hi

