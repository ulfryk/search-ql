/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';
import { None, Right, Some } from 'monet';

import { Expression, ExpressionType, ValueType } from '../../../common/model';
import { FunctionConfig, OptionalFunctionArg, ParserConfig, RequiredFunctionArg } from '../../../config';
import { ArgDemand } from '../../../dto';
import { AndOperator, LikeOperator } from '../../operators';
import { BinaryOperationExpression } from '../binary-operation';
import { NotExpression } from '../not';
import { DateExpression, fromMatch, PhraseExpression, TextExpression } from '../term';
import { FunctionExpression } from './function';

const config = new ParserConfig();

const fn = (name: string, ...args: Expression[]) =>
  new FunctionExpression(List(args), new FunctionConfig(
      name,
      List(args.map(({ returnType: t }, i) => OptionalFunctionArg.fromType(t, `arg${i}`))),
      None(),
      Right(ValueType.Boolean)));

const phrase = (val: string): PhraseExpression =>
  PhraseExpression.fromTerm(fromMatch(config)(val));

describe('SearchQL expressions', () => {

  describe('FunctionExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        fn('return_null'),
        fn('id', fromMatch(config)('lorem')),
        fn('is_date', fromMatch(config)('2018-01-01')),
      ];

      const rhs = [
        fn('return_null'),
        fn('id', fromMatch(config)('lorem')),
        fn('is_date', fromMatch(config)('2018-01-01')),
      ];

      const rhsInvalid = [
        fn('return_null', fromMatch(config)('lorem')),
        fn('idx', fromMatch(config)('lorem')),
        fn('is_date', fromMatch(config)('2018-01-02')),
      ];

      it('should return true for comparison with a reference', () => {
        lhs.forEach(fnExp => {
          expect(fnExp.equals(fnExp)).to.be.true;
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
        zip<Expression>(rhs, rhsInvalid).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.false;
        });
      });

    });

    describe('isValid() method', () => {

      const cfgTextIsDate = new FunctionConfig(
        'text_is_date', List([RequiredFunctionArg.fromType(ValueType.Text, 'text')]),
        None(), Right(ValueType.Boolean));

      const cfgIsDate = new FunctionConfig(
        'is_date', List([RequiredFunctionArg.fromType(ValueType.Any, 'input')]),
        None(), Right(ValueType.Boolean));

      const cfgTrim = new FunctionConfig(
        'trim', List([RequiredFunctionArg.fromType(ValueType.Text, 'text')]),
        None(), Right(ValueType.Text));

      const cfgCoalesce = new FunctionConfig(
        'coalesce', List([
          RequiredFunctionArg.fromType(ValueType.Text, 'option'),
          RequiredFunctionArg.fromType(ValueType.Text, 'option'),
        ]),
        Some(OptionalFunctionArg.fromType(ValueType.Text, 'option')),
        Right(ValueType.Text));

      const validFns = [
        FunctionExpression.fromParseResult(cfgIsDate, [new NotExpression(phrase('lorem'))]),
        FunctionExpression.fromParseResult(cfgIsDate, [
          new BinaryOperationExpression(new AndOperator('AND'), [
            phrase('lorem'),
            phrase('123'),
          ]),
        ]),
        FunctionExpression.fromParseResult(cfgTextIsDate, [fromMatch(config)('lorem')]),
        FunctionExpression.fromParseResult(cfgTextIsDate, [fromMatch(config)('2011-11-11')]),
        FunctionExpression.fromParseResult(cfgTextIsDate, [fromMatch(config)('123')]),
        FunctionExpression.fromParseResult(cfgTrim, [TextExpression.of('lorem')]),
        FunctionExpression.fromParseResult(cfgIsDate, [
          new BinaryOperationExpression(new LikeOperator('~'), [
            FunctionExpression.fromParseResult(cfgTrim, [TextExpression.of('   lorem  ')]),
            TextExpression.of('lorem'),
          ]),
        ]),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          TextExpression.of('lorem'),
          TextExpression.of('ipsum'),
        ]),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          TextExpression.of('lorem'),
          TextExpression.of('ipsum'),
          TextExpression.of('dolor'),
        ]),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          TextExpression.of('lorem'),
          TextExpression.of('ipsum'),
          FunctionExpression.fromParseResult(cfgTrim, [TextExpression.of('   lo re m  ')]),
          TextExpression.of('ipsum_x'),
          TextExpression.of('dolor'),
        ]),
        fn('return_null'),
        fn('id', TextExpression.of('lorem')),
        fn('is_date', DateExpression.of('2018-01-01')),
      ];

      it('should return true for valid function uses', () => {

        validFns.map(f => f.checkTypes()).forEach(checked => {
          expect(checked.isValid(), String(checked)).to.be.true;
        });

      });

      const invalidFns = [
        FunctionExpression.fromParseResult(cfgTextIsDate, [new NotExpression(fromMatch(config)('lorem'))]),
        new NotExpression(FunctionExpression.fromParseResult(cfgTrim, [fromMatch(config)('   lo re m  ')])),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          fromMatch(config)('lorem'),
        ]),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          fromMatch(config)('lorem'),
          fromMatch(config)('ipsum'),
          fromMatch(config)('lorem_x'),
          new NotExpression(fromMatch(config)('ipsum_x')),
          fromMatch(config)('dolor'),
        ]),
      ];

      it('should return false for valid function uses', () => {

        invalidFns.map(f => f.checkTypes()).forEach(checked => {
          expect(checked.isValid(), String(checked)).to.be.false;
        });

      });

    });

    describe('toList() method', () => {

      const lhs = [
        fn('return_null'),
        fn('id', fromMatch(config)('lorem')),
        fn('is_date', fromMatch(config)('2018-01-01')),
      ];

      const rhs = [
        List([lhs[0]]),
        List([lhs[1], lhs[1].value.first()]),
        List([lhs[2], lhs[2].value.first()]),
      ];

      it('should properly build up list of expressions', () => {
        zip<Expression, List<Expression>>(lhs, rhs).forEach(([left, right]) => {
          expect(left.toList().equals(right)).to.be.true;
        });
      });

    });

    describe('toJS() method', () => {

      const lhs = [
        fn('return_null'),
        fn('id', fromMatch(config)('lorem')),
        fn('is_date', fromMatch(config)('2018-01-01')),
      ];

      const rhs = [
        {
          config: {
            args: [] as any[],
            argsRest: null as any,
            name: 'return_null',
            returnType: ValueType.Boolean,
          },
          returnType: ValueType.Boolean,
          type: ExpressionType.Function,
          value: [] as any[],
        },
        {
          config: {
            args: [{
              demand: ArgDemand.Optional,
              expressionType: null,
              label: 'arg0',
              type: ValueType.Text,
              typeParam: null,
            }],
            argsRest: null as any,
            name: 'id',
            returnType: ValueType.Boolean,
          },
          returnType: ValueType.Boolean,
          type: ExpressionType.Function,
          value: [{
            returnType: ValueType.Text,
            type: ExpressionType.Text,
            value: 'lorem',
          }],
        },
        {
          config: {
            args: [{
              demand: ArgDemand.Optional,
              expressionType: null,
              label: 'arg0',
              type: ValueType.Date,
              typeParam: null,
            }],
            argsRest: null as any,
            name: 'is_date',
            returnType: ValueType.Boolean,
          },
          returnType: ValueType.Boolean,
          type: ExpressionType.Function,
          value: [{
            returnType: ValueType.Date,
            timeFrame: {
              end: 1514847599999,
              start: 1514761200000,
              text: '2018-01-01',
            },
            type: ExpressionType.Date,
            value: '2018-01-01',
          }],
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
