/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { List } from 'immutable';
import { zip } from 'lodash';

import { ValueType } from '../../../common/model';
import { Expression } from '../expression';
import { DateExpression, TermExpression, TextExpression } from '../term';
import { FunctionExpression } from './function';

describe('SearchQL expressions', () => {

  describe('FunctionExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        new FunctionExpression(List(), 'return_null', ValueType.Boolean),
        new FunctionExpression(List([TermExpression.fromMatch('lorem')]), 'id', ValueType.Boolean),
        new FunctionExpression(List([TermExpression.fromMatch('2018-01-01')]), 'is_date', ValueType.Boolean),
      ];

      const rhs = [
        new FunctionExpression(List(), 'return_null', ValueType.Boolean),
        new FunctionExpression(List([new TextExpression('lorem')]), 'id', ValueType.Boolean),
        new FunctionExpression(List([new DateExpression('2018-01-01')]), 'is_date', ValueType.Boolean),
      ];

      const rhsInvalid = [
        new FunctionExpression(List([TermExpression.fromMatch('lorem')]), 'return_null', ValueType.Boolean),
        new FunctionExpression(List([TermExpression.fromMatch('lorem')]), 'idx', ValueType.Boolean),
        new FunctionExpression(List([TermExpression.fromMatch('2018-01-02')]), 'is_date', ValueType.Boolean),
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
