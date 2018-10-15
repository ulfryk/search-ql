/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';

import { isDate } from './is-date';

const validInput = [
  '2018-11-21',
  new Date(Date.now() - 123123123).toJSON(),
  new Date(Date.now() - 8888899900).toISOString(),
  new Date(Date.now() + 5563400).toDateString(),
  'Aug 28 2018',
  '28 Aug 2018',
  'August 28, 2018',
];

const invalidInput = [
  '201811-21',
  'fasdfasdfasdf',
  '2018-11-21asdasd',
  '123123123',
  'Aug 282018',
  '$#%564 *(&^@%#1 1~`1`',
];

describe('SearchQL common/utils', () => {

  describe('isDate', () => {

    it('should be a function', () => {
      expect(isDate).to.be.instanceof(Function);
    });

    validInput.forEach(aDate => {
      it(`should return true for string that represents a Date: "${aDate}"`, () => {
        expect(isDate(aDate)).to.be.true;
      });
    });

    invalidInput.forEach(notADate => {
      it(`should return false for string that doesn't represent a Date: "${notADate}"`, () => {
        expect(isDate(notADate)).to.be.false;
      });
    });

  });

});
