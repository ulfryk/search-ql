/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map } from 'immutable';

import { DateExpression } from '../../ast';
import { isDate } from '../../ast/expressions/term/is-date';
import { SyntaxConfig } from '../../config';
import { DateExpressionTester } from './date-expression';

const config = new SyntaxConfig();

const getTester = (date: string) =>
  new DateExpressionTester(DateExpression.fromMatch(date), config);

describe('SearchQL testers', () => {

  describe('DateExpressionTester', () => {

    const values = Map<string, string>([
      ['firstName', 'John'],
      ['lastName', 'Doe'],
      ['createdAt', '1989-06-04'],
      ['updatedAt', 'Jun 18 1989 UTC'],
    ]);
    const matching = values.toArray().filter(isDate);
    const notMatching = ['Sun 13 Dec 1981', '1956-06-28'];

    matching.forEach(date => {
      it(`should find date "${date}"`, () => {
        expect(getTester(date).test(values).isSome()).to.be.true;
        expect(getTester(date).test(values).some().isEmpty()).to.be.false;
      });
    });

    notMatching.forEach(date => {
      it(`should not find date "${date}"`, () => {
        expect(getTester(date).test(values).isSome()).to.be.false;
      });
    });

    it('should find certain date in few fields', () => {
      const matches = getTester('1990-11-25').test(Map({
        five: '1990-11-25T08:11:00.000Z',
        four: 'Lorem ipsum',
        one: 'Sun Nov 25 1990 UTC',
        three: '1990-11-25T00:00:00.000Z',
        two: '1990 11 25 00:00 UTC',
      }));

      const stringified = 'Just(Map {' +
        ' "one": Match "Sun Nov 25 1990 UTC" { Map { "Sun Nov 25 1990 UTC": OrderedSet { [0, 19] } } },' +
        ' "three": Match "1990-11-25T00:00:00.000Z" { Map { "1990-11-25T00:00:00.000Z": OrderedSet { [0, 24] } } },' +
        ' "two": Match "1990 11 25 00:00 UTC" { Map { "1990 11 25 00:00 UTC": OrderedSet { [0, 20] } } }' +
      ' })';

      expect(String(matches)).to.equal(stringified);
    });

  });

});
