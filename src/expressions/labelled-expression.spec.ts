/* tslint:disable:no-unused-expression no-magic-numbers no-import-side-effect  */
import '@samwise-tech/immutable';
import { expect } from 'chai';
import { Map } from 'immutable';
import * as _ from 'lodash';

import { SyntaxConfig } from '../syntax-config';
import { BasicExpression } from './basic-expression';
import { Expression } from './expression';
import { LabelledExpression } from './labelled-expression';

const config = new SyntaxConfig();

describe('SearchQL expressions', () => {

  describe('LabelledExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        new LabelledExpression('label', BasicExpression.fromMatch('text')),
        new LabelledExpression('theLabel', BasicExpression.fromMatch('')),
        new LabelledExpression('', BasicExpression.fromMatch('text')),
      ];

      const rhs = [
        new LabelledExpression('label', new BasicExpression('text')),
        new LabelledExpression('theLabel', new BasicExpression('')),
        new LabelledExpression('', new BasicExpression('text')),
      ];

      const rhsInvalid = [
        new LabelledExpression('label', new BasicExpression('')),
        new LabelledExpression('label', new BasicExpression('')),
        new LabelledExpression('text', new BasicExpression('label')),
      ];

      it('should return true for comparison with a reference', () => {
        lhs.concat(rhs).concat(rhsInvalid).forEach(expression => {
          expect(expression.equals(expression)).to.be.true;
        });
      });

      it('should return true for different instances of same shape', () => {
        _.zip<Expression>(lhs, rhs).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.true;
        });
      });

      it('should return false for instances of different shape', () => {
        _.zip<Expression>(lhs, rhsInvalid).forEach(([left, right]) => {
          expect(left.equals(right)).to.be.false;
        });
      });

    });

    describe('test() method', () => {

      // tslint:disable-next-line:no-unnecessary-type-assertion
      const values = Map([
        'All good',
        'asdffa SDFAS sdf',
        ')((',
        'AND OR OR AND',
        'IpsUM-dolor_sitAMET',
        'hello world',
      ].map((val, i) => [`label ${i}`, val.toLowerCase()])) as Map<string, string>;

      const expressions = values
        .map((val: string) => val.substr(-6, 5))
        .entrySeq()
        .concat(values.entrySeq())
        .toArray()
        .map(([key, value]) => new LabelledExpression(key, new BasicExpression(value)));

      const notMatchingExpressions = values
        .map(val => `${val} additional text`)
        .entrySeq()
        .toArray()
        .map(([key, value]) => new LabelledExpression(key, new BasicExpression(value)));

      expressions.forEach(expression => {
        it(`should find expression "${expression}"`, () => {
          expect(expression.test(values, config).isSome()).to.be.true;
          expect(expression.test(values, config).some().isEmpty()).to.be.false;
        });
      });

      notMatchingExpressions.forEach(expression => {
        it(`should not find expression "${expression}"`, () => {
          expect(expression.test(values, config).isSome()).to.be.false;
        });
      });

    });

  });

});
