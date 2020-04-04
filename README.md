# svg-simplify

Do you have SVG files that render fine with some browsers or programs but render wrong in others?

This is due to software not supporting all of SVG's features.

Specifically, the ["mask"](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/mask) feature is not supported by several programs.

`svg-simplify` replaces masks with gradients **in some situations**. It may crash or still have bugs so be sure to verify its results!

## SVG features support

| Program      | `mask`
| ------------ | --------
| Chrome       | &#9989;
| Firefox      | &#9989;
| Safari       | &#10008;
| ImageMagick  | &#9989;
| rsvg-convert | &#9989;
| qlmanage     | &#10008;
| JUCE         | &#10008;
