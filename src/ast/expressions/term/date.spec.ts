/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';

import { Expression } from '../../../common/model';
import { DateExpression } from './date';
import { TermExpression } from './term';

describe('SearchQL expressions', () => {

  describe('DateExpression', () => {

    describe('of() static method', () => {

      it('should return instance of DateExpression', () => {
        expect(DateExpression.of('2011-01-01')).to.be.instanceof(TermExpression);
        expect(DateExpression.of('2011-01-01')).to.be.instanceof(DateExpression);
      });

    });

    describe('equals() method', () => {

      const dateA = new Date(Date.now() - 123123123);
      const dateB = new Date(Date.now() - 8888899900);
      const dateC = new Date(Date.now() + 5563400);

      const lhs = [
        DateExpression.of('Aug 28 2018'),
        DateExpression.of('28 Aug 2018'),
        DateExpression.of('28 August, 2018'),
        new DateExpression('Tue Aug 28 2018'),
        DateExpression.of('Jul 9 2018'),
        DateExpression.of('Jul 9 2018, 10:30'),
        DateExpression.of(dateA.toJSON()),
        DateExpression.of(dateB.toDateString()),
        DateExpression.of(String(dateC)),
      ];

      const rhs = [
        new DateExpression('2018-08-28'),
        new DateExpression('2018-08-28'),
        new DateExpression('2018-08-28'),
        new DateExpression('2018-08-28'),
        new DateExpression('2018-07-09'),
        new DateExpression('2018-07-09T10:30:00'),
        new DateExpression(dateA.toISOString()),
        new DateExpression(dateB.toDateString() + '   '),
        new DateExpression(dateC.toString()),
      ];

      const rhsInvalid = [
        DateExpression.of('15 Aug 2017'),
        new DateExpression('2000-01-01'),
        DateExpression.of('2000-01-01'),
        new DateExpression('2000-01-01'),
      ];

      lhs.concat(rhs).concat(rhsInvalid).forEach(expression => {
        it('should return true for comparison with a reference', () => {
          expect(expression.equals(expression), `${expression} should equal itself`).to.be.true;
        });
      });

      zip<DateExpression>(lhs, rhs).forEach(([left, right]) => {
        it(`should return true for different instances of same value (${left} vs ${right})`, () => {
          expect(
            left.equals(right),
            `${left.timeFrame} should equal ${right.timeFrame}`,
          ).to.be.true;
        });
      });

      zip<DateExpression>(lhs, rhsInvalid).forEach(([left, right]) => {
        it(`should return false for instances of different value  (${left} vs ${right})`, () => {
          expect(left.equals(right), `${left} should NOT equal ${right}`).to.be.false;
        });
      });

    });

    describe('toList() method', () => {

      const lhs = [
        new DateExpression('Tue Aug 28 2018'),
        DateExpression.of('2000-01-01'),
      ];

      const rhs = [
        List([lhs[0]]),
        List([lhs[1]]),
      ];

      it('should properly build up list of expressions', () => {
        zip<Expression, List<Expression>>(lhs, rhs).forEach(([left, right]) => {
          expect(left.toList().equals(right)).to.be.true;
        });
      });

    });

  });

});
