/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map } from 'immutable';

import { NumberExpression } from '../../ast';
import { isNumber } from '../../ast/expressions/term/is-number';
import { SyntaxConfig } from '../../config';
import { NumberExpressionTester } from './number-expression';

const config = new SyntaxConfig();

const getTester = (date: string) =>
  new NumberExpressionTester(NumberExpression.fromMatch(date), config);

describe('SearchQL testers', () => {

  describe('NumberExpressionTester', () => {

    const values = Map<string, string>([
      ['firstName', 'John'],
      ['lastName', 'Doe'],
      ['age', '29'],
      ['hourlyRate', '29.99'],
    ]);
    const matching = values.toArray().filter(isNumber).concat(['00029', '29.990000', '0x1D']);
    const notMatching = ['14', '51', '-29', '29.991'];

    matching.forEach(num => {
      it(`should find number "${num}"`, () => {
        expect(getTester(num).test(values).isSome()).to.be.true;
        expect(getTester(num).test(values).some().isEmpty()).to.be.false;
      });
    });

    notMatching.forEach(num => {
      it(`should not find number "${num}"`, () => {
        expect(getTester(num).test(values).isSome()).to.be.false;
      });
    });

    it('should find certain number in few fields', () => {
      const matches = getTester('-.51').test(Map({
        five: '51',
        four: 'Lorem ipsum',
        one: '-00.5100',
        three: '-0.51',
        two: '-0.051e1',
      }));

      const stringified = 'Just(Map {' +
        ' "one": Match "-00.5100" { Map { "-00.5100": OrderedSet { [0, 8] } } },' +
        ' "three": Match "-0.51" { Map { "-0.51": OrderedSet { [0, 5] } } },' +
        ' "two": Match "-0.051e1" { Map { "-0.051e1": OrderedSet { [0, 8] } } }' +
      ' })';

      expect(String(matches)).to.equal(stringified);
    });

  });

});
