/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map } from 'immutable';
import { zip } from 'lodash';

import { BasicExpression } from './basic-expression';
import { Expression } from './expression';

describe('SearchQL expressions', () => {

  describe('BasicExpression', () => {

    describe('fromMatch() static method', () => {

      it('should return instance of BasicExpression', () => {
        expect(BasicExpression.fromMatch('aaa')).to.be.instanceof(BasicExpression);
      });

    });

    describe('equals() method', () => {

      const lhs = [
        BasicExpression.fromMatch(''),
        BasicExpression.fromMatch('aaa  asdas as asd asdas dad '),
      ];

      const rhs = [
        new BasicExpression(''),
        new BasicExpression('aaa  asdas as asd asdas dad '),
      ];

      const rhsInvalid = [
        BasicExpression.fromMatch('aaa  asdas as asd asdas dad '),
        new BasicExpression(''),
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

      const expressions = values.toArray()
        .map((val: string) => val.substr(-6, 5))
        .concat(values.toArray())
        .map(BasicExpression.fromMatch);

      const notMatchingExpressions = values.toArray()
        .map((val, i) => `${i} ${val}`)
        .map(BasicExpression.fromMatch);

      expressions.forEach(expression => {
        it(`should find expression "${expression}"`, () => {
          expect(expression.test(values).isSome()).to.be.true;
          expect(expression.test(values).some().isEmpty()).to.be.false;
        });
      });

      notMatchingExpressions.forEach(expression => {
        it(`should not find expression "${expression}"`, () => {
          expect(expression.test(values).isSome()).to.be.false;
        });
      });

      it('should find expression "aaa" in few fields', () => {
        expect(
          BasicExpression
            .fromMatch('aaa')
            .test(Map({
              one: 'aaa bbb aaa aaa aaasda ddaaa',
              three: 'aaGaa bbb aadaaXaadaa ddddadd',
              two: 'aaa bbb aaaXaaa',
            }))
            .toString(),
        ).to.equal('Just(Map {' +
          ' "one": Match "aaa bbb aaa aaa aaasda ddaaa" {' +
            ' Map { "aaa": OrderedSet { [0, 3], [8, 11], [12, 15], [16, 19], [25, 28] } } },' +
          ' "two": Match "aaa bbb aaaXaaa" {' +
            ' Map { "aaa": OrderedSet { [0, 3], [8, 11], [12, 15] } } }' +
          ' })');
      });

    });

  });

});
