/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map } from 'immutable';
import * as _ from 'lodash';

import { BasicExpression } from './basic-expression';
import { Expression } from './expression';
import { NotExpression } from './not-expression';

describe('SearchQL expressions', () => {

  describe('NotExpression', () => {

    describe('equals() method', () => {

      const lhs = [
        new NotExpression(BasicExpression.fromMatch('')),
        new NotExpression(BasicExpression.fromMatch('aaa  asdas as asd asdas dad ')),
      ];

      const rhs = [
        new NotExpression(BasicExpression.fromMatch('')),
        new NotExpression(BasicExpression.fromMatch('aaa  asdas as asd asdas dad ')),
      ];

      const rhsInvalid = [
        new NotExpression(BasicExpression.fromMatch('aaa  asdas as asd asdas dad ')),
        new NotExpression(BasicExpression.fromMatch('')),
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

      const notMatchingExpressions = values.toArray()
        .map((val: string) => val.substr(-6, 5))
        .concat(values.toArray())
        .map(BasicExpression.fromMatch)
        .map(NotExpression.of);

      const expressions = values.toArray()
        .map((val, i) => `${i} ${val}`)
        .map(BasicExpression.fromMatch)
        .map(NotExpression.of);

      expressions.forEach(expression => {
        it(`should find expression "${expression}"`, () => {
          expect(expression.test(values).isSome()).to.be.true;
          // expect(expression.test(values).some().isEmpty()).to.be.true;
        });
      });

      notMatchingExpressions.forEach(expression => {
        it(`should not find expression "${expression}"`, () => {
          expect(expression.test(values).isSome()).to.be.false;
        });
      });

      it('should not find expression "zzz" in few fields', () => {
        expect(
          NotExpression.of(BasicExpression.fromMatch('zzz'))
            .test(Map({
              one: 'aaa bbb aaa aaa aaasda ddaaa',
              three: 'aaGaa bbb aadaaXaadaa ddddadd',
              two: 'aaa bbb aaaXaaa',
            }))
            .toString(),
        ).to.equal('Just(Map {' +
          ' "one": Match "aaa bbb aaa aaa aaasda ddaaa" { Map {} },' +
          ' "three": Match "aaGaa bbb aadaaXaadaa ddddadd" { Map {} },' +
          ' "two": Match "aaa bbb aaaXaaa" { Map {} }' +
          ' })');
      });

    });

  });

});
