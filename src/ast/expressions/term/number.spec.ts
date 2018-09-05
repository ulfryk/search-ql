/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';

import { Expression } from '../../../common/model';
import { NumberExpression } from './number';
import { TermExpression } from './term';

describe('SearchQL expressions', () => {

  describe('NumberExpression', () => {

    describe('of() static method', () => {

      it('should return instance of NumberExpression', () => {
        expect(NumberExpression.of('1')).to.be.instanceof(TermExpression);
        expect(NumberExpression.of('1')).to.be.instanceof(NumberExpression);
      });

    });

    describe('equals() method', () => {

      const lhs = [
        NumberExpression.of('1'),
        NumberExpression.of('2.00'),
        NumberExpression.of('003.00'),
        NumberExpression.of('4e2'),
        NumberExpression.of('-.01'),
      ];

      const rhs = [
        new NumberExpression('001'),
        new NumberExpression('002'),
        new NumberExpression('3'),
        new NumberExpression('400'),
        new NumberExpression('-0.01000'),
      ];

      const rhsInvalid = [
        NumberExpression.of('1.1'),
        new NumberExpression('0'),
        NumberExpression.of('3.01'),
        new NumberExpression('-400'),
        NumberExpression.of('-0.001'),
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

    describe('toList() method', () => {

      const lhs = [
        NumberExpression.of('12'),
        new NumberExpression('0'),
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
