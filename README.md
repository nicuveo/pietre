<div align="center">
  <img src="/docs/img/logo.png" alt="the project logo, which is itself a Piet program", style="float:left"/>

[Documentation](/docs) | [Twitch] <!-- | [Youtube] -->

</div>

<br />

Piètre is a small stack-based imperative language that compiles to [Piet](https://www.dangermouse.net/esoteric/piet.html), an esoteric language in which the source code is an image[^1]. Its syntax is similar to Rust's[^2].

The main goal of this project is to be educational: it aims first at being a good resource to learn about compilers and how to implement them. A secondary objective is to provide a reference for how to organize a medium-size Haskell codebase. Furthermore, until 1.0 is reached, the code of this project will be entirely written [live on Twitch][Twitch]. Streams are announced ahead of time on the [channel's schedule][Twitch schedule].

_Piètre_ means "shoddy" or "second-rate" in french.

<br />

## Getting started

This project is written in [Haskell] and uses [Stack]. To compile and test it[^3], you can run:

```bash
git clone https://github.com/nicuveo/pietre.git
cd pietre
stack build
stack run pc -- path/to/pietre/file.pi
```

Binary releases will be provided from 1.0 onwards.

<br />

## Contributing

Until 1.0 is reached, this project will remain a one-person effort and will not accept external contributions. There is no guarantee this policy will change past 1.0: this is a hobby project first and foremost. :)

<br />

## Roadmap

Once the project has made enough progress, a roadmap to 1.0 will be detailed here. Until the project can start, the remaining tasks are:
- [x] finish a draft version of the reference and implementation guides
- [ ] record a video announcing the project
- [ ] announce the project on social media
- [ ] schedule a first stream

<br />

[Twitch]: https://twitch.tv/nicuveo
[Youtube]: https://www.youtube.com/@nicuveo
[Twitch schedule]: https://www.twitch.tv/nicuveo/schedule
[Haskell]: https://www.haskell.org/
[Stack]: https://docs.haskellstack.org/en/stable/

[^1]: The logo of the project is itself a valid Piet program, that behaves like `cat`. You can try it with `echo "foo" | npiet -q logo.png`.
[^2]: But with fewer features. See the reference for more information.
[^3]: Once it works, that is.
