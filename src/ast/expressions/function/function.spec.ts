/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';
import { None, Some } from 'monet';

import { ValueType } from '../../../common/model';
import { FunctionConfig, OptionalFunctionArg, RequiredFunctionArg } from '../../../config';
import { AndOperator, LikeOperator } from '../../operators';
import { BinaryOperationExpression } from '../binary-operation';
import { Expression } from '../expression';
import { NotExpression } from '../not';
import { TermExpression } from '../term';
import { FunctionExpression } from './function';

// tslint:disable-next-line:no-unnecessary-callback-wrapper
const fakeRuntime = () => None<any>();

const fn = (name: string, ...args: Expression[]) =>
  new FunctionExpression(List(args), new FunctionConfig(
      name,
      List(args.map(({ returnType: t }, i) => OptionalFunctionArg.fromType(t, `arg${i}`))),
      None(),
      ValueType.Boolean,
      fakeRuntime));

describe('SearchQL expressions', () => {

  describe('FunctionExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        fn('return_null'),
        fn('id', TermExpression.fromMatch('lorem')),
        fn('is_date', TermExpression.fromMatch('2018-01-01')),
      ];

      const rhs = [
        fn('return_null'),
        fn('id', TermExpression.fromMatch('lorem')),
        fn('is_date', TermExpression.fromMatch('2018-01-01')),
      ];

      const rhsInvalid = [
        fn('return_null', TermExpression.fromMatch('lorem')),
        fn('idx', TermExpression.fromMatch('lorem')),
        fn('is_date', TermExpression.fromMatch('2018-01-02')),
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
        None(), ValueType.Boolean, fakeRuntime);

      const cfgIsDate = new FunctionConfig(
        'is_date', List([RequiredFunctionArg.fromType(ValueType.Any, 'input')]),
        None(), ValueType.Boolean, fakeRuntime);

      const cfgTrim = new FunctionConfig(
        'trim', List([RequiredFunctionArg.fromType(ValueType.Text, 'text')]),
        None(), ValueType.Text, fakeRuntime);

      const cfgCoalesce = new FunctionConfig(
        'coalesce', List([
          RequiredFunctionArg.fromType(ValueType.Text, 'option'),
          RequiredFunctionArg.fromType(ValueType.Text, 'option'),
        ]),
        Some(OptionalFunctionArg.fromType(ValueType.Text, 'option')),
        ValueType.Text, fakeRuntime);

      const validFns = [
        FunctionExpression.fromParseResult(cfgIsDate, [new NotExpression(TermExpression.fromMatch('lorem'))]),
        FunctionExpression.fromParseResult(cfgIsDate, [
          new BinaryOperationExpression(new AndOperator('AND'), [
            TermExpression.fromMatch('lorem'),
            TermExpression.fromMatch('123'),
          ]),
        ]),
        FunctionExpression.fromParseResult(cfgTextIsDate, [TermExpression.fromMatch('lorem')]),
        FunctionExpression.fromParseResult(cfgTextIsDate, [TermExpression.fromMatch('2011-11-11')]),
        FunctionExpression.fromParseResult(cfgTextIsDate, [TermExpression.fromMatch('123')]),
        FunctionExpression.fromParseResult(cfgTrim, [TermExpression.fromMatch('lorem')]),
        FunctionExpression.fromParseResult(cfgIsDate, [
          new BinaryOperationExpression(new LikeOperator('~'), [
            TermExpression.fromMatch('first_name'),
            FunctionExpression.fromParseResult(cfgTrim, [TermExpression.fromMatch('   lo re m  ')]),
          ]),
        ]),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          TermExpression.fromMatch('lorem'),
          TermExpression.fromMatch('ipsum'),
        ]),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          TermExpression.fromMatch('lorem'),
          TermExpression.fromMatch('ipsum'),
          TermExpression.fromMatch('dolor'),
        ]),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          TermExpression.fromMatch('lorem'),
          TermExpression.fromMatch('ipsum'),
          FunctionExpression.fromParseResult(cfgTrim, [TermExpression.fromMatch('   lo re m  ')]),
          TermExpression.fromMatch('ipsum_x'),
          TermExpression.fromMatch('dolor'),
        ]),
        fn('return_null'),
        fn('id', TermExpression.fromMatch('lorem')),
        fn('is_date', TermExpression.fromMatch('2018-01-01')),
      ];

      it('should return true for valid function uses', () => {

        validFns.map(f => f.checkTypes()).forEach(checked => {
          expect(checked.isValid(), String(checked)).to.be.true;
        });

      });

      const invalidFns = [
        FunctionExpression.fromParseResult(cfgTextIsDate, [new NotExpression(TermExpression.fromMatch('lorem'))]),
        new NotExpression(FunctionExpression.fromParseResult(cfgTrim, [TermExpression.fromMatch('   lo re m  ')])),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          TermExpression.fromMatch('lorem'),
        ]),
        FunctionExpression.fromParseResult(cfgCoalesce, [
          TermExpression.fromMatch('lorem'),
          TermExpression.fromMatch('ipsum'),
          TermExpression.fromMatch('lorem_x'),
          new NotExpression(TermExpression.fromMatch('ipsum_x')),
          TermExpression.fromMatch('dolor'),
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
        fn('id', TermExpression.fromMatch('lorem')),
        fn('is_date', TermExpression.fromMatch('2018-01-01')),
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
