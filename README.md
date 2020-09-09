# svg-simplify

Do you have SVG files that render fine with some browsers or programs but render wrong in others?

This is due to software not supporting all of SVG's features.

## Simplified features

* ["mask"](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/mask). `svg-simplify` replaces masks with gradients **in some situations**
* css rules for each class are grouped together (some engines don't support them being split over several rules)

## Installation

### Recommended: Build from source using haskell-stack

* Install [haskell stack](http://docs.haskellstack.org/en/stable/README/)
* Clone this project
* From the source folder, run `stack install`

## SVG features support

<table>
  <thead>
    <tr>
      <th colspan=2> SVG rendering feature support </th>
      <th> mask </th>
      <th> css rule aggregation </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th rowspan=3> Browsers </th>
      <th> Chrome </th>
      <td> &#9989; </td>
      <td> &#9989; </td>
    </tr>
    <tr>
      <th> Firefox </th>
      <td> &#9989; </td>
      <td> &#9989; </td>
    </tr>
    <tr>
      <th> Safari </th>
      <td> &#10008; </td>
      <td> &#9989; </td>
    </tr>
    <tr>
      <th rowspan=3> Drawing Apps </th>
      <th> Adobe Illustrator </th>
      <td> &#10008; </td>
      <td rowspan=6> TODO:<br/>check </td>
    </tr>
    <tr>
      <th> InkScape </th>
      <td> &#9989; </td>
    </tr>
    <tr>
      <th> SVG-edit </th>
      <td> &#10008; </td>
    </tr>
    <tr>
      <th rowspan=3> Conversion Tools </th>
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
    <tr>
      <th> Libraries </th>
      <th> JUCE </th>
      <td> &#10008; </td>
      <td> &#10008; </td>
    </tr>
    <tr>
      <th> Other </th>
      <th> VS Code Preview </th>
      <td> &#9989; </td>
      <td> &#9989; </td>
    </tr>
  </tbody>
</tr>
</table>
