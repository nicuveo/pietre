<div align="center">
  <img src="/docs/img/logo.png" alt="the project logo, which is itself a Piet program", style="float:left"/>

[Documentation](/docs) | [Twitch] | [YouTube]

</div>

<br />

Piètre is a small stack-based imperative language that compiles to
[Piet](https://www.dangermouse.net/esoteric/piet.html), an esoteric language in
which the source code is an image[^1]. Its syntax is similar to Rust's[^2]. You
can watch an introduction to the project on
[YouTube](https://youtu.be/uCQ2hjx_7-Y).

The main goal of this project is to be educational: it aims first at being a
good resource to learn about compilers and how to implement them, and a
secondary objective is to provide a reference for how to organize a medium-size
Haskell codebase. Furthermore, until 1.0 is reached, the code of this project
will be entirely written [live on Twitch][Twitch]; streams are announced ahead
of time on the [channel's schedule][Twitch schedule]. All streams are archived
on a dedicated [YouTube
channel](https://www.youtube.com/@nicuveo-archive/videos).

This live-coding project is part of the
[declarative.tv](https://declarative.tv/) initiative, where you can find other
streamers using functional and declarative languages. Join our Discord server to
chat about this project!

_Piètre_ means "shoddy" or "second-rate" in french.[^3]

<br />

## Status

[![main build status][ShieldM]][Actionm] [![dev build status][ShieldD]][ActionD] [![twitch status][TwitchStatus]][Twitch]

Until 1.0 is reached, all development will happen on the [dev branch](https://github.com/nicuveo/pietre/tree/dev).

<br />

## Getting started

This project is written in [Haskell] and uses [Stack]. To compile and test it[^4], you can run:

```bash
git clone https://github.com/nicuveo/pietre.git
cd pietre
stack build
stack run pc -- path/to/pietre/file.pi
```

Binary releases will be provided from 1.0 onwards.

<br />

## Contributing

Unless otherwise explicitly stated, this project does not accept external code
contributions. This policy *might* change after the 1.0 release, in which case a
code of conduct and a contributing guide will be added.

<br />

## Acknowledgements

Thank you to [@rond](https://github.com/rondDev) for fixing my clumsy attempt at JavaScript!

<br />

## Roadmap

Once the project has made enough progress, a roadmap to 1.0 will be detailed here.

<br />
<br />

[ActionM]: https://github.com/nicuveo/pietre/actions/workflows/validate.yml?query=branch%3Amain
[ActionD]: https://github.com/nicuveo/pietre/actions/workflows/validate.yml?query=branch%3Adev
[ShieldM]: https://img.shields.io/github/actions/workflow/status/nicuveo/pietre/validate.yml?logo=github&event=push&style=flat&branch=main&label=main%20build
[ShieldD]: https://img.shields.io/github/actions/workflow/status/nicuveo/pietre/validate.yml?logo=github&event=push&style=flat&branch=dev&label=dev%20build
[TwitchStatus]: https://img.shields.io/twitch/status/nicuveo?logo=twitch&logoColor=white&style=flat&cacheSeconds=300
[Twitch]: https://twitch.tv/nicuveo
[YouTube]: https://www.youtube.com/playlist?list=PLgbgEcpv1Xa21W1vEHnYO5mU70bWJW05j
[Twitch schedule]: https://www.twitch.tv/nicuveo/schedule
[Haskell]: https://www.haskell.org/
[Stack]: https://docs.haskellstack.org/en/stable/

[^1]: The logo of the project is itself a valid Piet program, that behaves like `cat`. You can try it with `echo "foo" | npiet -q logo.png`.
[^2]: But with fewer features. See the reference for more information.
[^3]: The intent is to signal that this project is not to be taken too seriously; it was also intended to be a pun, but it turns out I've been mispronouncing "Piet" for two decades. Whoops.
[^4]: Once it works, that is.
