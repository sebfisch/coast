# Coast - Code Host

Coast is a code hoast for the [Darcs] version control system. It is in a very
early stage of development and not ready for use.

If you want to try it anyway, you can get coast using darcs or git:

    > darcs get http://www-ps.informatik.uni-kiel.de/~sebf/darcs/coast
    > git clone git://github.com/sebfisch/coast.git

Then install [Yesod] and the dependencies of coast using [cabal-install]:

    > cabal install --only-dependencies

Finally, run coast on [http://localhost:3000]:

    > yesod devel

[Darcs]: http://darcs.net
[Yesod]: http://yesodweb.com
[cabal-install]: http://www.haskell.org/platform/
[http://localhost:3000]: http://localhost:3000

The homepage of coast lists all darcs repositories placed in the project
directory under `var/repos/`. If you got coast using darcs, you can browse it
in itself:

    > darcs get . var/repos/coast

# Why?

Coast is intended to be an alternative to [hub.darcs.net]. It uses different
technology under the hood and also looks different on the surface. It is based
on the [Yesod] web framework for Haskell, which I considered most suitable
after [comparing Yesod with Snap].

Coast is supported by [factis research GmbH] who want to use it to improve
their darcs-based code reviewing workflow.

[hub.darcs.net]: http://hub.darcs.net
[factis research GmbH]: http://www.factisresearch.com/index.html.en
[comparing Yesod with Snap]: https://gist.github.com/9f674bd869e2df09bb0e
