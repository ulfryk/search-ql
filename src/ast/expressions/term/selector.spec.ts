/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { Expression } from '../expression';
import { SelectorExpression } from './selector';
import { TermExpression } from './term';

describe('SearchQL expressions', () => {

  describe('SelectorExpression', () => {

    describe('fromMatch() static method', () => {

      it('should return instance of SelectorExpression', () => {
        expect(SelectorExpression.fromMatch('aaa')).to.be.instanceof(TermExpression);
        expect(SelectorExpression.fromMatch('aaa')).to.be.instanceof(SelectorExpression);
      });

    });

    describe('equals() method', () => {

      const lhs = [
        SelectorExpression.fromMatch('empty'),
        SelectorExpression.fromMatch('field_name'),
      ];

      const rhs = [
        new SelectorExpression('empty'),
        new SelectorExpression('field_name'),
      ];

      const rhsInvalid = [
        SelectorExpression.fromMatch('field_name'),
        new SelectorExpression('empty'),
      ];

      it('should return true for comparison with a reference', () => {
        lhs.concat(rhs).concat(rhsInvalid).forEach(expression => {
          expect(expression.equals(expression)).to.be.true;
        });
      });

      it('should return true for different instances of same shape', () => {
        zip<Expression>(lhs, rhs).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.true;
        });
      });

      it('should return false for instances of different shape', () => {
        zip<Expression>(lhs, rhsInvalid).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.false;
        });
      });

    });

  });

});
