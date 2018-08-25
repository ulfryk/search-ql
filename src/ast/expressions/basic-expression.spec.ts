/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { BasicExpression } from './basic-expression';
import { Expression } from './expression';

describe('SearchQL expressions', () => {

  describe('BasicExpression', () => {

    describe('fromMatch() static method', () => {

      it('should return instance of BasicExpression', () => {
        expect(BasicExpression.fromMatch('aaa')).to.be.instanceof(BasicExpression);
      });

    });

    describe('equals() method', () => {

      const lhs = [
        BasicExpression.fromMatch(''),
        BasicExpression.fromMatch('aaa  asdas as asd asdas dad '),
      ];

      const rhs = [
        new BasicExpression(''),
        new BasicExpression('aaa  asdas as asd asdas dad '),
      ];

      const rhsInvalid = [
        BasicExpression.fromMatch('aaa  asdas as asd asdas dad '),
        new BasicExpression(''),
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
