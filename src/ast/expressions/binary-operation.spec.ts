/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';

import { Expression } from '../../common/model';
import { ParserConfig } from '../../config';
import { AndOperator, IsNotOperator, IsOperator, LikeOperator, OrOperator } from '../operators';
import { BinaryOperationExpression } from './binary-operation';
import { NumberExpression, PhraseExpression, TextExpression } from './term';

const { AND, IS, IS_NOT, LIKE, OR } = new ParserConfig();
const And = new AndOperator(AND[0]);
const Is = new IsOperator(IS[0]);
const IsNot = new IsNotOperator(IS_NOT[0]);
const Like = new LikeOperator(LIKE[0]);
const Or = new OrOperator(OR[0]);

describe('SearchQL expressions', () => {

  describe('BinaryOperationExpression', () => {

    describe('fromPair() method', () => {

      it('should properly create similarity expressions (LIKE)', () => {
        expect(BinaryOperationExpression.fromPair(Like)(
          new TextExpression('aaa'),
          new NumberExpression('123'),
        )).to.deep.equal(new BinaryOperationExpression(Like, [
          new TextExpression('aaa'),
          PhraseExpression.of('123'),
        ]));
      });

      it('should properly create equality expressions (IS, IS_NOT)', () => {

        expect(BinaryOperationExpression.fromPair(Is)(
          TextExpression.of('age'),
          NumberExpression.of('123'),
        )).to.deep.equal(new BinaryOperationExpression(Is, [
          TextExpression.of('age'),
          PhraseExpression.of('123'),
        ]));

        expect(BinaryOperationExpression.fromPair(IsNot)(
          PhraseExpression.of('age'),
          NumberExpression.of('123'),
        )).to.deep.equal(new BinaryOperationExpression(IsNot, [
          TextExpression.of('age'),
          PhraseExpression.of('123'),
        ]));

      });

      it('should properly create logical expressions (AND, OR)', () => {

        expect(BinaryOperationExpression.fromPair(And)(
          new NumberExpression('123'),
          new TextExpression('aaa'),
        )).to.deep.equal(new BinaryOperationExpression(And, [
          PhraseExpression.of('123'),
          PhraseExpression.of('aaa'),
        ]));

        expect(BinaryOperationExpression.fromPair(Or)(
          new TextExpression('aaa'),
          new NumberExpression('123'),
        )).to.deep.equal(new BinaryOperationExpression(Or, [
          PhraseExpression.of('aaa'),
          PhraseExpression.of('123'),
        ]));

      });
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

    describe('toList() method', () => {

      const lhs = [
        new BinaryOperationExpression(Like, [
          new TextExpression('aaa'),
          new TextExpression('123'),
        ]),
        new BinaryOperationExpression(And, [
          new TextExpression('123'),
          new TextExpression('aaa'),
        ]),
        new BinaryOperationExpression(Or, [
          new TextExpression('aaa'),
          new TextExpression('123'),
        ]),
      ];

      const rhs = [
        List([lhs[0], lhs[0].value[0], lhs[0].value[1]]),
        List([lhs[1], lhs[1].value[0], lhs[1].value[1]]),
        List([lhs[2], lhs[2].value[0], lhs[2].value[1]]),
      ];

      it('should properly build up list of expressions', () => {
        zip<Expression, List<Expression>>(lhs, rhs).forEach(([left, right]) => {
          expect(left.toList().equals(right)).to.be.true;
        });
      });

    });

  });

});
