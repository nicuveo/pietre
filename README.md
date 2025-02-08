<div align="center">
  <img src="/docs/img/logo.png" alt="the project logo, which is itself a Piet program", style="float:left"/>

[Documentation](/docs) | [Twitch] | [Youtube]

</div>

<br />

Piètre is a small stack-based imperative language that compiles to [Piet](https://www.dangermouse.net/esoteric/piet.html), an esoteric language in which the source code is an image[^1]. Its syntax is similar to Rust's[^2].

The main goal of this project is to be educational: it aims first at being a good resource to learn about compilers and how to implement them. A secondary objective is to provide a reference for how to organize a medium-size Haskell codebase. Furthermore, until 1.0 is reached, the code of this project will be entirely written [live on Twitch][Twitch]. Streams are announced ahead of time on the [channel's schedule][Twitch schedule].

_Piètre_ means "shoddy" or "second-rate" in french.[^3]

<br />

## Status

[![main build status][ShieldM]][Actionm] [![dev build status][ShieldD]][ActionD] <br />
[![twitch status][TwitchStatus]][Twitch]


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

Until 1.0 is reached, this project will remain a one-person effort and will not accept external contributions. There is no guarantee this policy will change past 1.0: this is a hobby project first and foremost. :)

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
[Youtube]: https://www.youtube.com/playlist?list=PLgbgEcpv1Xa21W1vEHnYO5mU70bWJW05j
[Twitch schedule]: https://www.twitch.tv/nicuveo/schedule
[Haskell]: https://www.haskell.org/
[Stack]: https://docs.haskellstack.org/en/stable/

[^1]: The logo of the project is itself a valid Piet program, that behaves like `cat`. You can try it with `echo "foo" | npiet -q logo.png`.
[^2]: But with fewer features. See the reference for more information.
[^3]: The intent is to signal that this project is not to be taken too seriously; it was also intended to be a pun, but it turns out I've been mispronouncing "Piet" for two decades. Whoops.
[^4]: Once it works, that is.
