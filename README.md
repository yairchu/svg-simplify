# svg-simplify

Do you have SVG files that render fine with some browsers or programs but render wrong in others?

This is due to software not supporting all of SVG's features.

Specifically, the ["mask"](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/mask) feature is not supported by several programs.

`svg-simplify` replaces masks with gradients **in some situations**. It may crash or still have bugs so be sure to verify its results!

## Installation

### Recommended: Build from source using haskell-stack

* Install [haskell stack](http://docs.haskellstack.org/en/stable/README/)
* Clone this project
* From the source folder, run `stack install`

## SVG features support

<table>
  <style>
    td { text-align: center; }
  </style>
  <thead>
    <tr>
      <th colspan=2>
        SVG rendering feature support
      </th>
      <th style="font-family: monospace; text-align: center">
        mask
      </th>
    </tr>
  </thead>
  <tbody>
    <tr style="border-top: solid">
      <th rowspan=3>
        Browsers
      </th>
      <th> Chrome </th>
      <td> &#9989; </td>
    </tr>
    <tr>
      <th> Firefox </th>
      <td> &#9989; </td>
    </tr>
    <tr>
      <th> Safari </th>
      <td> &#10008; </td>
    </tr>
    <tr style="border-top: solid">
      <th rowspan=2>
        Drawing Apps
      </th>
      <th> Adobe Illustrator </th>
      <td> &#10008; </td>
    </tr>
    <tr>
      <th> InkScape </th>
      <td> &#9989; </td>
    </tr>
    <tr style="border-top: solid">
      <th rowspan=3>
        Conversion Tools
      </th>
      <th> ImageMagick </th>
      <td> &#9989; </td>
    </tr>
    <tr>
      <th> rsvg-convert </th>
      <td> &#9989; </td>
    </tr>
    <tr>
      <th> qlmanage </th>
      <td> &#10008; </td>
    </tr>
    <tr style="border-top: solid">
      <th> Libraries </th>
      <th> JUCE </th>
      <td> &#10008; </td>
    </tr>
    <tr style="border-top: solid">
      <th> Other </th>
      <th> VS Code Preview </th>
      <td> &#9989; </td>
    </tr>
  </tbody>
</tr>
</table>
