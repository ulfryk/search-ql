/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
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

    describe('toList() method', () => {

      const lhs = [
        SelectorExpression.fromMatch(''),
        SelectorExpression.fromMatch('aaa  asdas as asd asdas dad '),
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
