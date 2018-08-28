/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { DateExpression } from './date';
import { TermExpression } from './term';

describe('SearchQL expressions', () => {

  describe('DateExpression', () => {

    describe('fromMatch() static method', () => {

      it('should return instance of DateExpression', () => {
        expect(DateExpression.fromMatch('1')).to.be.instanceof(TermExpression);
        expect(DateExpression.fromMatch('1')).to.be.instanceof(DateExpression);
      });

    });

    describe('equals() method', () => {

      const dateA = new Date(Date.now() - 123123123);
      const dateB = new Date(Date.now() - 8888899900);
      const dateC = new Date(Date.now() + 5563400);

      const lhs = [
        DateExpression.fromMatch('28 Aug 2018'),
        DateExpression.fromMatch('9 Jul 2018 GMT'),
        DateExpression.fromMatch(dateA.toJSON()),
        DateExpression.fromMatch(dateB.toDateString()),
        DateExpression.fromMatch(String(dateC)),
      ];

      const rhs = [
        new DateExpression('Tue Aug 28 2018'),
        new DateExpression('2018-07-09'),
        new DateExpression(dateA.toISOString()),
        new DateExpression(dateB.toDateString() + '   '),
        new DateExpression(dateC.toString()),
      ];

      const rhsInvalid = [
        DateExpression.fromMatch('15 Aug 2017'),
        new DateExpression('Tue Aug 28 2018'),
        new DateExpression('2000-01-01'),
        DateExpression.fromMatch('2000-01-01'),
        new DateExpression('2000-01-01'),
      ];

      it('should return true for comparison with a reference', () => {
        lhs.concat(rhs).concat(rhsInvalid).forEach(expression => {
          expect(expression.equals(expression)).to.be.true;
        });
      });

      it('should return true for different instances of same value', () => {
        zip<DateExpression>(lhs, rhs).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.true;
        });
      });

      it('should return false for instances of different value', () => {
        zip<DateExpression>(lhs, rhsInvalid).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.false;
        });
      });

    });

  });

});
