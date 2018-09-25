/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';

import { Expression, ExpressionType, ValueType } from '../../common/model';
import { ParserConfig } from '../../config';
import { NotExpression } from './not';
import { fromMatch, PhraseExpression } from './term';

const config = new ParserConfig();
const phrase = (val: string) => PhraseExpression.fromTerm(fromMatch(config)(val));

describe('SearchQL expressions', () => {

  describe('NotExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        new NotExpression(phrase('')),
        new NotExpression(phrase('aaa  asdas as asd asdas dad ')),
      ];

      const rhs = [
        new NotExpression(phrase('')),
        new NotExpression(phrase('aaa  asdas as asd asdas dad ')),
      ];

      const rhsInvalid = [
        new NotExpression(phrase('aaa  asdas as asd asdas dad ')),
        new NotExpression(phrase('')),
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
        new NotExpression(phrase('')),
        new NotExpression(phrase('aaa  asdas as asd asdas dad ')),
      ];

      const rhs = [
        List([lhs[0], lhs[0].value, (lhs[0].value as PhraseExpression<any>).term]),
        List([lhs[1], lhs[1].value, (lhs[1].value as PhraseExpression<any>).term]),
      ];

      it('should properly build up list of expressions', () => {
        zip<Expression, List<Expression>>(lhs, rhs).forEach(([left, right]) => {
          expect(left.toList().equals(right)).to.be.true;
        });
      });

    });

    describe('toJS() method', () => {

      const lhs = [
        new NotExpression(phrase('')),
        new NotExpression(phrase('aaa  asdas as asd asdas dad ')),
      ];

      const rhs = [
        {
          returnType: 'PHRASE',
          type: ExpressionType.Not,
          value: {
            preparedValue: '',
            returnType: ValueType.Phrase,
            term: {
              preparedValue: '',
              returnType: ValueType.Text,
              type: ExpressionType.Text,
              value: '',
            },
            type: ExpressionType.Phrase,
            value: '',
          },
        },
        {
          returnType: 'PHRASE',
          type: ExpressionType.Not,
          value: {
            preparedValue: 'aaa  asdas as asd asdas dad',
            returnType: ValueType.Phrase,
            term: {
              preparedValue: 'aaa  asdas as asd asdas dad',
              returnType: ValueType.Text,
              type: ExpressionType.Text,
              value: 'aaa  asdas as asd asdas dad ',
            },
            type: ExpressionType.Phrase,
            value: 'aaa  asdas as asd asdas dad ',
          },
        },
      ];

      it('should properly build up JSON', () => {
        zip(lhs, rhs).forEach(([left, right]) => {
          expect(left.toJS()).to.deep.equal(right);
        });
      });

    });

  });

});
