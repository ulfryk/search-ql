/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';
import { None, Some } from 'monet';

import { Expression, ValueType } from '../../../common/model';
import { FunctionConfig, OptionalFunctionArg, RequiredFunctionArg, testFunction } from '../../../config';
import { AndOperator, LikeOperator } from '../../operators';
import { BinaryOperationExpression } from '../binary-operation';
import { NotExpression } from '../not';
import { DateExpression, fromMatch, TermExpression, TextExpression } from '../term';
import { FunctionExpression } from './function';

const fn = (name: string, ...args: Expression[]) =>
  new FunctionExpression(List(args), new FunctionConfig(
      name,
      List(args.map(({ returnType: t }, i) => OptionalFunctionArg.fromType(t, `arg${i}`))),
      None(),
      ValueType.Boolean,
      testFunction.runtime));

describe('SearchQL expressions', () => {

  describe('FunctionExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        fn('return_null'),
        fn('id', fromMatch('lorem')),
        fn('is_date', fromMatch('2018-01-01')),
      ];

      const rhs = [
        fn('return_null'),
        fn('id', fromMatch('lorem')),
        fn('is_date', fromMatch('2018-01-01')),
      ];

      const rhsInvalid = [
        fn('return_null', fromMatch('lorem')),
        fn('idx', fromMatch('lorem')),
        fn('is_date', fromMatch('2018-01-02')),
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
        None(), ValueType.Boolean, testFunction.runtime);

      const cfgIsDate = new FunctionConfig(
        'is_date', List([RequiredFunctionArg.fromType(ValueType.Any, 'input')]),
        None(), ValueType.Boolean, testFunction.runtime);

      const cfgTrim = new FunctionConfig(
        'trim', List([RequiredFunctionArg.fromType(ValueType.Text, 'text')]),
        None(), ValueType.Text, testFunction.runtime);

      const cfgCoalesce = new FunctionConfig(
        'coalesce', List([
          RequiredFunctionArg.fromType(ValueType.Text, 'option'),
          RequiredFunctionArg.fromType(ValueType.Text, 'option'),
        ]),
        Some(OptionalFunctionArg.fromType(ValueType.Text, 'option')),
        ValueType.Text, testFunction.runtime);

      const validFns = [
        FunctionExpression.fromParseResult(cfgIsDate, [new NotExpression(TermExpression.of('lorem'))]),
        FunctionExpression.fromParseResult(cfgIsDate, [
          new BinaryOperationExpression(new AndOperator('AND'), [
            TermExpression.of('lorem'),
            TermExpression.of('123'),
          ]),
        ]),
        FunctionExpression.fromParseResult(cfgTextIsDate, [fromMatch('lorem')]),
        FunctionExpression.fromParseResult(cfgTextIsDate, [fromMatch('2011-11-11')]),
        FunctionExpression.fromParseResult(cfgTextIsDate, [fromMatch('123')]),
        FunctionExpression.fromParseResult(cfgTrim, [TextExpression.of('lorem')]),
        FunctionExpression.fromParseResult(cfgIsDate, [
          new BinaryOperationExpression(new LikeOperator('~'), [
            FunctionExpression.fromParseResult(cfgTrim, [TextExpression.of('   first_name  ')]),
            TermExpression.of('lorem'),
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
        FunctionExpression.fromParseResult(cfgTextIsDate, [new NotExpression(fromMatch('lorem'))]),
        new NotExpression(FunctionExpression.fromParseResult(cfgTrim, [fromMatch('   lo re m  ')])),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          fromMatch('lorem'),
        ]),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          fromMatch('lorem'),
          fromMatch('ipsum'),
          fromMatch('lorem_x'),
          new NotExpression(fromMatch('ipsum_x')),
          fromMatch('dolor'),
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
        fn('id', fromMatch('lorem')),
        fn('is_date', fromMatch('2018-01-01')),
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

  });

});
