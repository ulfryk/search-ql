/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';

import { Expression } from '../../common/model';
import { NotExpression } from './not';
import { PhraseExpression } from './term';

describe('SearchQL expressions', () => {

  describe('NotExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        new NotExpression(PhraseExpression.of('')),
        new NotExpression(PhraseExpression.of('aaa  asdas as asd asdas dad ')),
      ];

      const rhs = [
        new NotExpression(PhraseExpression.of('')),
        new NotExpression(PhraseExpression.of('aaa  asdas as asd asdas dad ')),
      ];

      const rhsInvalid = [
        new NotExpression(PhraseExpression.of('aaa  asdas as asd asdas dad ')),
        new NotExpression(PhraseExpression.of('')),
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
        new NotExpression(PhraseExpression.of('')),
        new NotExpression(PhraseExpression.of('aaa  asdas as asd asdas dad ')),
      ];

      const rhs = [
        List([lhs[0], lhs[0].value]),
        List([lhs[1], lhs[1].value]),
      ];

      it('should properly build up list of expressions', () => {
        zip<Expression, List<Expression>>(lhs, rhs).forEach(([left, right]) => {
          expect(left.toList().equals(right)).to.be.true;
        });
      });

    });

  });

});
