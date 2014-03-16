Main: Main.hs Application.hs Routes.hs PathHelpers.hs Records.hs Federation.hs MustacheTemplates.hs
	ghc -threaded -O2 -Wall -fno-warn-name-shadowing Main.hs

Routes.hs: routes
	routeGenerator -r -m Application -n 4 $< > $@

PathHelpers.hs: routes
	routeGenerator -cp -n 4 $< > $@

MustacheTemplates.hs: Records.hs view/showAccount.mustache view/header.mustache view/meta.mustache
	mustache2hs -m Records.hs view/showAccount.mustache ShowAccount > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main Routes.hs PathHelpers.hs MustacheTemplates.hs
