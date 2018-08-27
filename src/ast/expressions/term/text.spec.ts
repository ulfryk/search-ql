/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { Expression } from '../expression';
import { TextExpression } from './text';

describe('SearchQL expressions', () => {

  describe('TextExpression', () => {

    describe('fromMatch() static method', () => {

      it('should return instance of TextExpression', () => {
        expect(TextExpression.fromMatch('aaa')).to.be.instanceof(TextExpression);
      });

    });

    describe('equals() method', () => {

      const lhs = [
        TextExpression.fromMatch(''),
        TextExpression.fromMatch('aaa  asdas as asd asdas dad '),
      ];

      const rhs = [
        new TextExpression(''),
        new TextExpression('aaa  asdas as asd asdas dad '),
      ];

      const rhsInvalid = [
        TextExpression.fromMatch('aaa  asdas as asd asdas dad '),
        new TextExpression(''),
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
