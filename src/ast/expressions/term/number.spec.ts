/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { Expression } from '../expression';
import { NumberExpression } from './number';
import { TermExpression } from './term';

describe('SearchQL expressions', () => {

  describe('NumberExpression', () => {

    describe('fromMatch() static method', () => {

      it('should return instance of NumberExpression', () => {
        expect(NumberExpression.fromMatch('1')).to.be.instanceof(TermExpression);
        expect(NumberExpression.fromMatch('1')).to.be.instanceof(NumberExpression);
      });

    });

    describe('equals() method', () => {

      const lhs = [
        NumberExpression.fromMatch('1'),
        NumberExpression.fromMatch('2.00'),
        NumberExpression.fromMatch('003.00'),
        NumberExpression.fromMatch('4e2'),
        NumberExpression.fromMatch('-.01'),
      ];

      const rhs = [
        new NumberExpression('001'),
        new NumberExpression('002'),
        new NumberExpression('3'),
        new NumberExpression('400'),
        new NumberExpression('-0.01000'),
      ];

      const rhsInvalid = [
        NumberExpression.fromMatch('1.1'),
        new NumberExpression('0'),
        NumberExpression.fromMatch('3.01'),
        new NumberExpression('-400'),
        NumberExpression.fromMatch('-0.001'),
      ];

      it('should return true for comparison with a reference', () => {
        lhs.concat(rhs).concat(rhsInvalid).forEach(expression => {
          expect(expression.equals(expression)).to.be.true;
        });
      });

      it('should return true for different instances of same value', () => {
        zip<Expression>(lhs, rhs).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.true;
        });
      });

      it('should return false for instances of different value', () => {
        zip<Expression>(lhs, rhsInvalid).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.false;
        });
      });

    });

  });

});
