/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';

import { isNumber } from './is-number';

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
const invalidInput = [
  '1s234',
  'asdfasdf',
  '123.123.123',
  '123,43',
  '',
  '0.0.0',
  '--0',
];

describe('SearchQL expressions', () => {

  describe('isNumber', () => {

    it('should be a function', () => {
      expect(isNumber).to.be.instanceof(Function);
    });

    validInput.forEach(aNumber => {
      it(`should return true for string that represents a Number: "${aNumber}"`, () => {
        expect(isNumber(aNumber)).to.be.true;
      });
    });

    invalidInput.forEach(notANumber => {
      it(`should return false for string that doesn't represent a Number: "${notANumber}"`, () => {
        expect(isNumber(notANumber)).to.be.false;
      });
    });

  });

});
