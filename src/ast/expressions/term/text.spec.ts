/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';

import { Expression } from '../../../common/model';
import { TermExpression } from './term';
import { TextExpression } from './text';

describe('SearchQL expressions', () => {

  describe('TextExpression', () => {

    describe('of() static method', () => {

      it('should return instance of TextExpression', () => {
        expect(TextExpression.of('aaa')).to.be.instanceof(TermExpression);
        expect(TextExpression.of('aaa')).to.be.instanceof(TextExpression);
      });

    });

    describe('equals() method', () => {

      const lhs = [
        TextExpression.of(''),
        TextExpression.of('aaa  asdas as asd asdas dad '),
      ];

      const rhs = [
        new TextExpression(''),
        new TextExpression('aaa  asdas as asd asdas dad '),
      ];

      const rhsInvalid = [
        TextExpression.of('aaa  asdas as asd asdas dad '),
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

    describe('toList() method', () => {

      const lhs = [
        TextExpression.of(''),
        TextExpression.of('aaa  asdas as asd asdas dad '),
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
