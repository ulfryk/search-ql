/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';
import { None } from 'monet';

import { ValueType } from '../../../common/model';
import { FunctionConfig, OptionalFunctionArg } from '../../../config';
import { Expression } from '../expression';
import { TermExpression } from '../term';
import { FunctionExpression } from './function';

const fn = (name: string, ...args: Expression[]) =>
  new FunctionExpression(List(args), new FunctionConfig(
      name,
      List(args.map(({ returnType: t }, i) => OptionalFunctionArg.fromType(t, `arg${i}`))),
      None(),
      ValueType.Boolean,
      // tslint:disable-next-line:no-unnecessary-callback-wrapper
      () => None()));

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

  });

});
