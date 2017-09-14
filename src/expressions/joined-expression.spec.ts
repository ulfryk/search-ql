/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Map, Set } from 'immutable';
import * as _ from 'lodash';

import { AND, LogicOperator, OR } from '../syntax-config';
import { BasicExpression } from './basic-expression';
import { Expression } from './expression';
import { JoinedExpression } from './joined-expression';

describe('SearchQL expressions', () => {

  describe('JoinedExpression', () => {

    let i = 111; // tslint:disable-line:no-let
    const b = () => new BasicExpression('aaa' + i++); // Make sure values are not equal

    const initialAnd = new JoinedExpression(AND, Set<Expression>([b(), b()]));
    const initialOr = new JoinedExpression(OR, Set<Expression>([b(), b()]));

    describe('add() method', () => {

      it('should return instance of JoinedExpression', () => {
        expect(initialAnd.add(AND, b())).to.be.instanceof(JoinedExpression);
        expect(initialAnd.add(OR, b())).to.be.instanceof(JoinedExpression);
      });

      describe('should return JoinedExpression ', () => {

        it('with added value if same type passed', () => {
          expect(initialAnd.value.size).to.equal(2);
          expect(initialAnd.add(AND, b()).add(AND, b()).value.size).to.equal(4);

          expect(initialOr.value.size).to.equal(2);
          expect(initialOr.add(OR, b()).add(OR, b()).add(OR, b()).value.size).to.equal(5);
        });

        it('with initial and passed expressions as values if opposite type passed', () => {
          const additional = b();
          const output = (type: LogicOperator, original: JoinedExpression) =>
            new JoinedExpression(type, Set([original, additional]));

          expect(initialAnd.add(OR, additional).equals(output(OR, initialAnd))).to.be.true;
          expect(initialOr.add(AND, additional).equals(output(AND, initialOr))).to.be.true;
        });

      });

    });

    describe('equals() method', () => {

      const toAdd = [b(), b()];

      const lhs = [
        JoinedExpression.emptyAnd().add(OR, JoinedExpression.emptyOr()),
        initialAnd.add(AND, toAdd[0]).add(AND, toAdd[1]),
      ];

      const rhs = [
        new JoinedExpression(OR, Set([
          JoinedExpression.emptyAnd(),
          JoinedExpression.emptyOr(),
        ])),
        new JoinedExpression(AND, Set(initialAnd.value
          .map(({ value }) => new BasicExpression(value))
          .concat([
            new BasicExpression(toAdd[0].value),
            new BasicExpression(toAdd[1].value),
          ]))),
      ];

      const rhsInvalid = [
        new JoinedExpression(OR, Set([
          JoinedExpression.emptyAnd(),
          JoinedExpression.emptyAnd(),
        ])),
        new JoinedExpression(AND, Set(initialAnd.value
          .map(({ value }) => new BasicExpression(value))
          .concat([
            new BasicExpression(toAdd[0].value),
            new BasicExpression(toAdd[0].value),
          ]))),
      ];

      it('should return true for comparison with a reference', () => {
        expect(initialAnd.equals(initialAnd)).to.be.true;
        expect(initialOr.equals(initialOr)).to.be.true;
        expect(rhs[0].equals(rhs[0])).to.be.true;
        expect(lhs[1].equals(lhs[1])).to.be.true;
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
      ].map((val, j) => [`label ${j}`, val.toLowerCase()])) as Map<string, string>;

      const expressions = [
        new JoinedExpression(AND, Set(['All', 'good'].map(BasicExpression.fromMatch))),
        new JoinedExpression(AND, Set(['asdffa', 'SDFAS', 'sdf'].map(BasicExpression.fromMatch))),
        new JoinedExpression(AND, Set(['AND', 'OR', 'AND'].map(BasicExpression.fromMatch))),
        new JoinedExpression(OR, Set(['dolor_sitAMET', 'world'].map(BasicExpression.fromMatch))),
        new JoinedExpression(OR, Set(['xyz', 'zyx', 'ello wo'].map(BasicExpression.fromMatch))),
      ];

      const notMatchingExpressions = [
        new JoinedExpression(AND, Set(['All', 'is good'].map(BasicExpression.fromMatch))),
        new JoinedExpression(AND, Set(['asdffa', 'SDFAS', 'sdfx'].map(BasicExpression.fromMatch))),
        new JoinedExpression(AND, Set(['AND', 'OR', 'OOxx'].map(BasicExpression.fromMatch))),
        new JoinedExpression(OR, Set(['dolor--sitAMET', 'worldx'].map(BasicExpression.fromMatch))),
        new JoinedExpression(OR, Set(['xyz', 'zyx', 'ello woxx'].map(BasicExpression.fromMatch))),
      ];

      expressions.forEach(expression => {
        it(`should find expression "${expression}"`, () => {
          expect(expression.test(values)).to.be.true;
        });
      });

      notMatchingExpressions.forEach(expression => {
        it(`should not find expression "${expression}"`, () => {
          expect(expression.test(values)).to.be.false;
        });
      });

    });

  });

});
