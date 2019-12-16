# Example Apps

These are some very basic single file example yesod apps. All taken from the
[yesod book](https://www.yesodweb.com/book-1.6/).

Code is mostly as-is from the book with my own comments and slight modifications in few places while trying things out. This is done for my own `lean by typing code` and should not be treated as any learning resource. Read the book!

This repo contains `stack.yml` which uses system ghc. Install `yesod` in global namespace using:

```sh
stack install yesod yesod-bin
```

Then run each example separately using `runghc` like:

```sh
stack runghc 01-helloworld.hs
```

This will start a server on port [http://localhost:3000](http://localhost:3000).
