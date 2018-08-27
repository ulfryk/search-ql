/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { AndOperator, OrOperator } from '../operators';
import { BinaryOperationExpression } from './binary-operation';
import { Expression } from './expression';
import { TextExpression } from './term';

describe('SearchQL expressions', () => {

  describe('BinaryOperationExpression', () => {

    let i = 111; // tslint:disable-line:no-let
    const b = () => new TextExpression('aaa' + i++); // Make sure values are not equal

    const and = new BinaryOperationExpression(AndOperator.one, [b(), b()]);
    const or = new BinaryOperationExpression(OrOperator.one, [b(), b()]);

    describe('equals() method', () => {

      const toAdd: [TextExpression, TextExpression] = [b(), b()];

      const lhs = [
        new BinaryOperationExpression(OrOperator.one, [and, or]),
        or,
        and,
      ];

      const rhs = [
        new BinaryOperationExpression(OrOperator.one, [
          new BinaryOperationExpression(AndOperator.one, and.value),
          new BinaryOperationExpression(OrOperator.one, or.value),
        ]),
        new BinaryOperationExpression(OrOperator.one, or.value),
        new BinaryOperationExpression(AndOperator.one, and.value),
      ];

      const rhsInvalid = [
        new BinaryOperationExpression(OrOperator.one, [
          new BinaryOperationExpression(AndOperator.one, or.value),
          new BinaryOperationExpression(OrOperator.one, and.value),
        ]),
        new BinaryOperationExpression(AndOperator.one, toAdd),
        new BinaryOperationExpression(AndOperator.one, toAdd),
      ];

      it('should return true for comparison with a reference', () => {
        expect(and.equals(and)).to.be.true;
        expect(or.equals(or)).to.be.true;
        expect(rhs[0].equals(rhs[0])).to.be.true;
        expect(lhs[1].equals(lhs[1])).to.be.true;
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
