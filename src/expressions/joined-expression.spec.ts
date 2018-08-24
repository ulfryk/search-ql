/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Set } from 'immutable';
import { zip } from 'lodash';

import { SyntaxConfig } from '../syntax-config';
import { BasicExpression } from './basic-expression';
import { Expression } from './expression';
import { JoinedExpression } from './joined-expression';

const config = new SyntaxConfig();
const { AND, OR } = config;

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
          const output = (type: string, original: JoinedExpression) =>
            new JoinedExpression(type, Set([original, additional]));

          expect(initialAnd.add(OR, additional).equals(output(OR, initialAnd))).to.be.true;
          expect(initialOr.add(AND, additional).equals(output(AND, initialOr))).to.be.true;
        });

      });

    });

    describe('equals() method', () => {

      const toAdd = [b(), b()];

      const lhs = [
        JoinedExpression.empty(AND).add(OR, JoinedExpression.empty(OR)),
        initialAnd.add(AND, toAdd[0]).add(AND, toAdd[1]),
      ];

      const rhs = [
        new JoinedExpression(OR, Set([
          JoinedExpression.empty(AND),
          JoinedExpression.empty(OR),
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
          JoinedExpression.empty(AND),
          JoinedExpression.empty(AND),
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

  });

});
