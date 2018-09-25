/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';

import { Expression, ExpressionType, OperatorType, ValueType } from '../../common/model';
import { ParserConfig } from '../../config';
import { AndOperator, GteOperator, GtOperator, IsNotOperator, IsOperator, LikeOperator, LteOperator, LtOperator, NotLikeOperator, OrOperator } from '../operators';
import { BinaryOperationExpression } from './binary-operation';
import { fromMatch, NumberExpression, PhraseExpression, SelectorExpression, TextExpression } from './term';

const config = new ParserConfig();
const { AND, GT, GTE, IS, IS_NOT, LIKE, LT, LTE, NOT_LIKE, OR } = config;
const And = new AndOperator(AND[0]);
const Gt = new GtOperator(GT[0]);
const Gte = new GteOperator(GTE[0]);
const Is = new IsOperator(IS[0]);
const IsNot = new IsNotOperator(IS_NOT[0]);
const Like = new LikeOperator(LIKE[0]);
const Lt = new LtOperator(LT[0]);
const Lte = new LteOperator(LTE[0]);
const NotLike = new NotLikeOperator(NOT_LIKE[0]);
const Or = new OrOperator(OR[0]);
const phrase = (val: string) => PhraseExpression.fromTerm(fromMatch(config)(val));

describe('SearchQL expressions', () => {

  describe('BinaryOperationExpression', () => {

    describe('fromPair() static method', () => {

      it('should properly create similarity expressions (LIKE, NOT_LIKE)', () => {

        expect(BinaryOperationExpression.fromPair(Like)(
          SelectorExpression.of('aaa')(ValueType.Number),
          new NumberExpression('123'),
        )).to.deep.equal(new BinaryOperationExpression(Like, [
          SelectorExpression.of('aaa')(ValueType.Number),
          NumberExpression.of('123'),
        ]));

        expect(BinaryOperationExpression.fromPair(NotLike)(
          NumberExpression.of('123'),
          SelectorExpression.of('aaa')(ValueType.Number),
        )).to.deep.equal(new BinaryOperationExpression(NotLike, [
          new NumberExpression('123'),
          SelectorExpression.of('aaa')(ValueType.Number),
        ]));

        expect(BinaryOperationExpression.fromPair(Like)(
          new NumberExpression('321'),
          new NumberExpression('123'),
        )).to.deep.equal(new BinaryOperationExpression(Like, [
          new NumberExpression('321'),
          new NumberExpression('123'),
        ]));

        expect(BinaryOperationExpression.fromPair(NotLike)(
          new NumberExpression('321'),
          new NumberExpression('123'),
        )).to.deep.equal(new BinaryOperationExpression(NotLike, [
          new NumberExpression('321'),
          new NumberExpression('123'),
        ]));

      });

      it('should properly create equality expressions (IS, IS_NOT)', () => {

        expect(BinaryOperationExpression.fromPair(Is)(
          SelectorExpression.of('age')(ValueType.Number),
          NumberExpression.of('123'),
        )).to.deep.equal(new BinaryOperationExpression(Is, [
          SelectorExpression.of('age')(ValueType.Number),
          new NumberExpression('123'),
        ]));

        expect(BinaryOperationExpression.fromPair(IsNot)(
          TextExpression.of('age'),
          new NumberExpression('123'),
        )).to.deep.equal(new BinaryOperationExpression(IsNot, [
          TextExpression.of('age'),
          NumberExpression.of('123'),
        ]));

        expect(BinaryOperationExpression.fromPair(IsNot)(
          NumberExpression.of('123'),
          SelectorExpression.of('age')(ValueType.Number),
        )).to.deep.equal(new BinaryOperationExpression(IsNot, [
          new NumberExpression('123'),
          SelectorExpression.of('age')(ValueType.Number),
        ]));

      });

      it('should properly create logical expressions (AND, OR)', () => {

        expect(BinaryOperationExpression.fromPair(And)(
          new NumberExpression('123'),
          new TextExpression('aaa'),
        )).to.deep.equal(new BinaryOperationExpression(And, [
          phrase('123'),
          phrase('aaa'),
        ]));

        expect(BinaryOperationExpression.fromPair(Or)(
          new TextExpression('aaa'),
          new NumberExpression('123'),
        )).to.deep.equal(new BinaryOperationExpression(Or, [
          phrase('aaa'),
          phrase('123'),
        ]));

      });

      it('should properly create relational expressions (GT, LT, GTE, LTE)', () => {

        expect(BinaryOperationExpression.fromPair(Gt)(
          SelectorExpression.of('age')(ValueType.Number),
          NumberExpression.of('123'),
        )).to.deep.equal(new BinaryOperationExpression(Gt, [
          SelectorExpression.of('age')(ValueType.Number),
          NumberExpression.of('123'),
        ]));

        expect(BinaryOperationExpression.fromPair(Lte)(
          NumberExpression.of('123'),
          SelectorExpression.of('age')(ValueType.Number),
        )).to.deep.equal(new BinaryOperationExpression(Lte, [
          NumberExpression.of('123'),
          SelectorExpression.of('age')(ValueType.Number),
        ]));

        expect(BinaryOperationExpression.fromPair(Gte)(
          TextExpression.of('age'),
          NumberExpression.of('123'),
        )).to.deep.equal(new BinaryOperationExpression(Gte, [
          TextExpression.of('age'),
          NumberExpression.of('123'),
        ]));

        expect(BinaryOperationExpression.fromPair(Lt)(
          NumberExpression.of('123'),
          TextExpression.of('age'),
        )).to.deep.equal(new BinaryOperationExpression(Lt, [
          NumberExpression.of('123'),
          TextExpression.of('age'),
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

    describe('toJS() method', () => {

      const lhs = [
        new BinaryOperationExpression(Like, [
          new TextExpression('aaa'),
          new NumberExpression('123'),
        ]),
        new BinaryOperationExpression(And, [
          new PhraseExpression(new TextExpression('123')),
          new PhraseExpression(new TextExpression('aaa')),
        ]),
        new BinaryOperationExpression(Is, [
          new TextExpression('aaa'),
          new TextExpression('123'),
        ]),
      ];

      const rhs = [
        {
          operator: { type: OperatorType.Like, token: LIKE[0] },
          returnType: ValueType.Boolean,
          type: ExpressionType.Binary,
          value: [
            { preparedValue: 'aaa', returnType: ValueType.Text, type: ExpressionType.Text, value: 'aaa' },
            { preparedValue: 123, returnType: ValueType.Number, type: ExpressionType.Number, value: '123' },
          ],
        },
        {
          operator: { type: OperatorType.And, token: AND[0] },
          returnType: ValueType.Phrase,
          type: ExpressionType.Binary,
          value: [
            {
              preparedValue: '123',
              returnType: ValueType.Phrase,
              term: {
                preparedValue: '123',
                returnType: ValueType.Text,
                type: ExpressionType.Text,
                value: '123',
              },
              type: ExpressionType.Phrase,
              value: '123',
            },
            {
              preparedValue: 'aaa',
              returnType: ValueType.Phrase,
              term: {
                preparedValue: 'aaa',
                returnType: ValueType.Text,
                type: ExpressionType.Text,
                value: 'aaa',
              },
              type: ExpressionType.Phrase,
              value: 'aaa',
            },
          ],
        },
        {
          operator: { type: OperatorType.Is, token: IS[0] },
          returnType: ValueType.Boolean,
          type: ExpressionType.Binary,
          value: [
            { preparedValue: 'aaa', returnType: ValueType.Text, type: ExpressionType.Text, value: 'aaa' },
            { preparedValue: '123', returnType: ValueType.Text, type: ExpressionType.Text, value: '123' },
          ],
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
