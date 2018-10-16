/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { parseNumber } from './parse-number';

const validInput = [
  '1234',
  '0',
  '-0',
  '-4235435345',
  '000032',
  '1',
  '-1',
  '10e343',
  '0x432',
  '-1.123',
  '543.4564567',
  '.123',
  '-.123',
  '.0',
  '-.0000',
];

const parsedOutput = [
  '1234',
  '0',
  '0',
  '-4235435345',
  '32',
  '1',
  '-1',
  '1e+344',
  '1074',
  '-1.123',
  '543.4564567',
  '0.123',
  '-0.123',
  '0',
  '0',
];

const invalidInput = [
  '1s234',
  'asdfasdf',
  '123.123.123',
  '123,43',
  '',
  '0.0.0',
  '--0',
];

describe('SearchQL common/utils', () => {

  describe('parseNumber', () => {

    it('should be a function', () => {
      expect(parseNumber).to.be.instanceof(Function);
    });

    zip(validInput, parsedOutput).forEach(([aNumber, parsed]) => {
      it(`should return parsed string that represents a Number: "${aNumber}" -> "${parsed}"`,
        () => {
          expect(parseNumber(aNumber).toString()).to.equal(parsed);
        });
    });

    invalidInput.forEach(notANumber => {
      it(`should throw for a string that doesn't represent a Number: "${notANumber}"`, () => {
        expect(() => { parseNumber(notANumber); }).to.throw('[big.js] Invalid number');
      });
    });

  });

});
