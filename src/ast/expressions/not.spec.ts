/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { Expression } from './expression';
import { NotExpression } from './not';
import { TextExpression } from './term';

describe('SearchQL expressions', () => {

  describe('NotExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        new NotExpression(TextExpression.fromMatch('')),
        new NotExpression(TextExpression.fromMatch('aaa  asdas as asd asdas dad ')),
      ];

      const rhs = [
        new NotExpression(TextExpression.fromMatch('')),
        new NotExpression(TextExpression.fromMatch('aaa  asdas as asd asdas dad ')),
      ];

      const rhsInvalid = [
        new NotExpression(TextExpression.fromMatch('aaa  asdas as asd asdas dad ')),
        new NotExpression(TextExpression.fromMatch('')),
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
