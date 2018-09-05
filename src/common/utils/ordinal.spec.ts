/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';

import { toOrdinal } from './ordinal';

const input = [
  [0, '0th'],
  [1, '1st'],
  [2, '2nd'],
  [3, '3rd'],
  [4, '4th'],
  [7, '7th'],
  [8, '8th'],
  [9, '9th'],
  [10, '10th'],
  [11, '11th'],
  [12, '12th'],
  [13, '13th'],
  [14, '14th'],
  [15, '15th'],
  [16, '16th'],
  [17, '17th'],
  [18, '18th'],
  [19, '19th'],
  [20, '20th'],
  [21, '21st'],
  [22, '22nd'],
  [23, '23rd'],
  [24, '24th'],
  [25, '25th'],
  [26, '26th'],
  [27, '27th'],
  [28, '28th'],
  [64, '64th'],
  [65, '65th'],
  [66, '66th'],
  [67, '67th'],
  [68, '68th'],
  [69, '69th'],
  [70, '70th'],
  [71, '71st'],
  [72, '72nd'],
  [73, '73rd'],
  [89, '89th'],
  [90, '90th'],
  [91, '91st'],
  [92, '92nd'],
  [93, '93rd'],
  [94, '94th'],
  [98, '98th'],
  [99, '99th'],
  [100, '100th'],
  [101, '101st'],
  [102, '102nd'],
  [103, '103rd'],
  [104, '104th'],
  [108, '108th'],
  [109, '109th'],
  [110, '110th'],
  [111, '111th'],
  [112, '112th'],
  [113, '113th'],
  [115, '115th'],
];

describe('SearchQL', () => {

  describe('common/utils/ordinal', () => {

    it(`should properly convert number to ordinal (String)`, () => {

      input.forEach(([n, o]: [number, string]) => {
        expect(toOrdinal(n)).to.equal(o);
      });

    });

  });

});
