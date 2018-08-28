/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { SyntaxConfig } from '../../config/syntax-config';
import { AndOperator, OrOperator } from '../operators';
import { BinaryOperationExpression } from './binary-operation';
import { Expression } from './expression';
import { TextExpression } from './term';

const { AND, OR } = new SyntaxConfig();
const And = new AndOperator(AND);
const Or = new OrOperator(OR);

describe('SearchQL expressions', () => {

  describe('BinaryOperationExpression', () => {

    xdescribe('fromPair() method', () => {
      xit('should properly create similarity expressions (LIKE)', () => { /* */ });
      xit('should properly create logical expressions (AND, OR)', () => { /* */ });
    });

    describe('equals() method', () => {

      let i = 111; // tslint:disable-line:no-let
      const b = () => new TextExpression('aaa' + i++); // Make sure values are not equal

      const and = new BinaryOperationExpression(And, [b(), b()]);
      const or = new BinaryOperationExpression(Or, [b(), b()]);

      const toAdd: [TextExpression, TextExpression] = [b(), b()];

      const lhs = [
        new BinaryOperationExpression(Or, [and, or]),
        or,
        and,
      ];

      const rhs = [
        new BinaryOperationExpression(Or, [
          new BinaryOperationExpression(And, and.value),
          new BinaryOperationExpression(Or, or.value),
        ]),
        new BinaryOperationExpression(Or, or.value),
        new BinaryOperationExpression(And, and.value),
      ];

      const rhsInvalid = [
        new BinaryOperationExpression(Or, [
          new BinaryOperationExpression(And, or.value),
          new BinaryOperationExpression(Or, and.value),
        ]),
        new BinaryOperationExpression(And, toAdd),
        new BinaryOperationExpression(And, toAdd),
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
