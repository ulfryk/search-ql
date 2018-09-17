/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';

import { Expression, ValueType } from '../../common/model';
import { and, And, andNot, config, is, Is, isNot, IsNot, Like, like, Not, NotLike, notLike, or, Or, sel } from '../../testing/utils';
import { fromMatch, NotExpression, PhraseExpression, TextExpression } from '../expressions';
import { Operator } from '../operators';
import { BinaryOperationChain } from './binary-operation-chain';

const buildChain = (initial: Expression, ...chain: [Operator, Expression][]) =>
  chain.reduce(
    (acc, [op, expr]) => acc.append(expr, config)(op),
    BinaryOperationChain.init(initial));

const testReshapeMethod = (chain: BinaryOperationChain, expected: Expression) => {
  const expression = chain.reshape();

  describe(`chain: ${chain}`, () => {

    it('should be instance of Expression', () => {
      expect(expression).to.be.instanceOf(Expression);
      expect(expression).to.be.instanceOf(expected.constructor);
      expect(expression).to.not.be.instanceOf(BinaryOperationChain);
    });

    it('should build proper Expression AST based on precedence and associativity of operators', () => {
      expect(expression).to.deep.equal(expected);
    });

  });
};

describe('SearchQL ast', () => {

  describe('BinaryOperationChain', () => {

    it('should be defined', () => {
      expect(BinaryOperationChain).to.be.instanceOf(Function);
    });

    describe('init() static method', () => {
      const initial = fromMatch(config)('1234');
      const group = BinaryOperationChain.init(initial);

      it('should create BinaryOperationChain instance', () => {
        expect(group).to.be.instanceOf(Expression);
        expect(group).to.be.instanceOf(BinaryOperationChain);
      });

      it('should create BinaryOperationChain with single expression', () => {
        expect(group.value.length).to.equal(1);
        expect(group.value[0]).to.equal(initial);
      });

      it('should create BinaryOperationChain without any operators', () => {
        expect(group.operators.size).to.equal(0);
      });

    });

    describe('appendBinary() method', () => {
      const initial = fromMatch(config)('lorem');
      const second = fromMatch(config)('ipsum');
      const third = fromMatch(config)('dolor');
      const group = BinaryOperationChain.init(initial);
      const group2 = group.appendBinary(And, second);
      const group3 = group2.appendBinary(Or, third);

      it('should create BinaryOperationChain instance', () => {
        expect(group2).to.be.instanceOf(BinaryOperationChain);
        expect(group3).to.be.instanceOf(BinaryOperationChain);
      });

      it('should append another operator and rhs to the group in immutable manner', () => {
        expect(group.value.length).to.equal(1);
        expect(group.operators.size).to.equal(0);
        expect(group2.value.length).to.equal(2);
        expect(group2.operators.size).to.equal(1);
        expect(group3.value.length).to.equal(3);
        expect(group3.operators.size).to.equal(2);
      });

      it('should keep expressions order unchanged', () => {
        expect(group.value[0]).to.equal(initial);
        expect(group.value[0]).to.equal(group2.value[0]);
        expect(group.value[0]).to.equal(group3.value[0]);
        expect(group2.value[1]).to.equal(second);
        expect(group2.value[1]).to.equal(group3.value[1]);
        expect(group3.value[2]).to.equal(third);
      });

      it('should keep operators order unchanged', () => {
        expect(group2.operators.first().operator).to.equal(And);
        expect(group2.operators.first().index).to.equal(0);
        expect(group2.operators.first()).to.equal(group3.operators.first());
        expect(group3.operators.last().operator).to.equal(Or);
        expect(group3.operators.last().index).to.equal(1);
      });

    });

    describe('appendAndNot() method', () => {
      const initial = fromMatch(config)('lorem');
      const second = fromMatch(config)('ipsum');
      const group = BinaryOperationChain.init(initial);
      const andNotGroup = group.appendAndNot(config, second);

      it('should create BinaryOperationChain instance', () => {
        expect(andNotGroup).to.be.instanceOf(BinaryOperationChain);
      });

      it('should append conjunction of negated expression', () => {
        expect(andNotGroup.operators.last().operator.equals(And)).to.be.true;
        expect(andNotGroup.value[1]).to.be.instanceOf(NotExpression);
        expect(andNotGroup.value[1].value.equals(PhraseExpression.fromTerm(second))).to.be.true;
      });

    });

    describe('append() method', () => {
      const initial = fromMatch(config)('lorem');
      const second = fromMatch(config)('ipsum');
      const third = fromMatch(config)('dolor');
      const fourth = fromMatch(config)('amet');

      const group = BinaryOperationChain.init(initial);
      const group2 = group.append(second, config)(And);
      const group3 = group2.append(third, config)(Not);
      const group4 = group3.append(fourth, config)(Or);

      it('should create BinaryOperationChain instance', () => {
        expect(group2).to.be.instanceOf(BinaryOperationChain);
        expect(group3).to.be.instanceOf(BinaryOperationChain);
        expect(group4).to.be.instanceOf(BinaryOperationChain);
      });

      it('should preserve expressions order', () => {
        expect(group2.value[1]).to.equal(second);
        expect(group2.value[1]).to.equal(group3.value[1]);
        expect(group2.value[1]).to.equal(group4.value[1]);
        expect(group3.value[2].value).to.equal(group4.value[2].value);
      });

      it('should preserve operators order', () => {
        expect(group2.operators.first()).to.equal(group3.operators.first());
        expect(group2.operators.first()).to.equal(group4.operators.first());
      });

      it('should properly handle NOT operator', () => {
        expect(group3.operators.last().operator.equals(And)).to.be.true;
        expect(group3.value[2]).to.be.instanceOf(NotExpression);
        expect(group4.value[2].value.equals(PhraseExpression.fromTerm(third))).to.be.true;
      });

    });

    describe('equals() method', () => {

      const group1 = BinaryOperationChain.init(fromMatch(config)('lorem'));
      const group2 = group1.append(fromMatch(config)('ipsum'), config)(And);
      const group3 = group2.append(fromMatch(config)('dolor'), config)(Not);
      const group3a = BinaryOperationChain.init(TextExpression.of('lorem'))
        .append(TextExpression.of('ipsum'), config)(And)
        .append(TextExpression.of('dolor'), config)(Not);
      const group4 = group3.append(fromMatch(config)('sit amet'), config)(Or);
      const group4a = BinaryOperationChain.init(fromMatch(config)('lorem'))
        .append(TextExpression.of('ipsum'), config)(And)
        .append(fromMatch(config)('dolor'), config)(Not)
        .append(TextExpression.of('sit amet'), config)(Or);

      it('should return false when comparing different shape instances', () => {
        expect(group1.equals(group2)).to.be.false;
        expect(group1.equals(group3)).to.be.false;
        expect(group1.equals(group4)).to.be.false;
        expect(group2.equals(group3)).to.be.false;
        expect(group2.equals(group4)).to.be.false;
        expect(group3.equals(group4)).to.be.false;
      });

      it('should return true when comparing to own reference', () => {
        expect(group1.equals(group1)).to.be.true;
        expect(group2.equals(group2)).to.be.true;
        expect(group3.equals(group3)).to.be.true;
        expect(group4.equals(group4)).to.be.true;
      });

      it('should return true when comparing different instances of the same shape', () => {
        expect(group3.equals(group3a)).to.be.true;
        expect(group4.equals(group4a)).to.be.true;
      });

    });

    describe('toString() method', () => {
      const group = BinaryOperationChain.init(fromMatch(config)('lorem'))
        .append(TextExpression.of('ipsum'), config)(And)
        .append(fromMatch(config)('dolor'), config)(Not)
        .append(TextExpression.of('sit amet'), config)(Or);

      it('should properly stringify group', () => {
        expect(group.toString()).to.equal('"lorem" & "ipsum" AND NOT "dolor" | "sit amet"');
      });

    });

    describe('reshape() method', () => {

      describe('for single expression', () => {
        const a = fromMatch(config)('aaaa');

        testReshapeMethod(buildChain(a), a);

      });

      describe('for And-only chains', () => {
        const [a, b, c, d, e, f] = ['Aaaa', 'Bbbb', 'Cccc', 'Dddd', 'Eeee', 'Ffff']
          .map(fromMatch(config))
          .map(PhraseExpression.fromTerm);

        const chain2 = buildChain(a, [And, b]);
        const chain3 = buildChain(a, [And, b], [And, c]);
        const chain4 = buildChain(a, [And, b], [And, c], [And, d]);
        const chain5 = buildChain(a, [And, b], [And, c], [And, d], [And, e]);
        const chain6 = buildChain(a, [And, b], [And, c], [And, d], [And, e], [And, f]);

        const expression2 = and(a, b);
        const expression3 = and(expression2, c);
        const expression4 = and(expression3, d);
        const expression5 = and(expression4, e);
        const expression6 = and(expression5, f);

        testReshapeMethod(chain2, expression2);
        testReshapeMethod(chain3, expression3);
        testReshapeMethod(chain4, expression4);
        testReshapeMethod(chain5, expression5);
        testReshapeMethod(chain6, expression6);

      });

      describe('for Or-only chains', () => {
        const [a, b, c, d, e, f] = ['Aaaa', 'Bbbb', 'Cccc', 'Dddd', 'Eeee', 'Ffff']
          .map(fromMatch(config))
          .map(PhraseExpression.fromTerm);

        const chain2 = buildChain(a, [Or, b]);
        const chain3 = buildChain(a, [Or, b], [Or, c]);
        const chain4 = buildChain(a, [Or, b], [Or, c], [Or, d]);
        const chain5 = buildChain(a, [Or, b], [Or, c], [Or, d], [Or, e]);
        const chain6 = buildChain(a, [Or, b], [Or, c], [Or, d], [Or, e], [Or, f]);

        const expression2 = or(a, b);
        const expression3 = or(expression2, c);
        const expression4 = or(expression3, d);
        const expression5 = or(expression4, e);
        const expression6 = or(expression5, f);

        testReshapeMethod(chain2, expression2);
        testReshapeMethod(chain3, expression3);
        testReshapeMethod(chain4, expression4);
        testReshapeMethod(chain5, expression5);
        testReshapeMethod(chain6, expression6);

      });

      describe('for mixed And/Or chains', () => {
        const [a, b, c, d, e, f] = ['Aaaa', 'Bbbb', 'Cccc', 'Dddd', 'Eeee', 'Ffff']
          .map(fromMatch(config))
          .map(PhraseExpression.fromTerm);

        const chainA = buildChain(a, [Or, b], [And, c], [Or, d]);
        const chainB = buildChain(a, [Or, b], [And, c], [Or, d], [And, e], [Or, f]);
        const chainC = buildChain(a, [And, b], [Or, c]);
        const chainD = buildChain(a, [And, b], [Or, c], [And, d], [Or, e], [And, f]);
        const chainE = buildChain(a, [Or, b], [Or, c], [And, d], [Or, e], [Or, f]);

        const expressionA = or(or(a, and(b, c)), d);
        const expressionB = or(or(or(a, and(b, c)), and(d, e)), f);
        const expressionC = or(and(a, b), c);
        const expressionD = or(or(and(a, b), and(c, d)), and(e, f));
        const expressionE = or(or(or(or(a, b), and(c, d)), e), f);

        testReshapeMethod(chainA, expressionA);
        testReshapeMethod(chainB, expressionB);
        testReshapeMethod(chainC, expressionC);
        testReshapeMethod(chainD, expressionD);
        testReshapeMethod(chainE, expressionE);
      });

      describe('for mixed chains', () => {
        const [a, b, c, d, e, f] = ['Aaaa', 'Bbbb', 'Cccc', 'Dddd', 'Eeee', 'Ffff']
          .map(fromMatch(config))
          .map(PhraseExpression.fromTerm);

        const [sa, sc, sd, se] = [a, c, d, e]
          .map(({ value }) => sel(value, ValueType.Text));

        const chain0 = buildChain(sa, [Is, b]);
        const chainA = buildChain(sa, [Like, b], [And, sc], [Like, d], [Or, se], [Like, f]);
        const chainB = buildChain(sa, [Like, b], [Not, c], [Or, sd], [Like, e], [Not, f]);
        const chainC = buildChain(sa, [Is, b], [And, sc], [IsNot, d]);
        const chainD = buildChain(sa, [Is, b], [And, sc], [IsNot, d], [Or, sa], [IsNot, b]);
        const chainE = buildChain(
          sa, [Is, b], [And, sc], [IsNot, d],
          [Or, sa], [IsNot, b], [And, sc], [Is, d]);
        const chainF = buildChain(sa, [NotLike, b], [And, sc], [Like, d], [Or, se], [NotLike, f]);

        const expression0 = is(sa, b);
        const expressionA = or(and(like(sa, b), like(sc, d)), like(se, f));
        const expressionB = or(andNot(like(sa, b), c), andNot(like(sd, e), f));
        const expressionC = and(is(sa, b), isNot(sc, d));
        const expressionD = or(and(is(sa, b), isNot(sc, d)), isNot(sa, b));
        const expressionE = or(and(is(sa, b), isNot(sc, d)), and(isNot(sa, b), is(sc, d)));
        const expressionF = or(and(notLike(sa, b), like(sc, d)), notLike(se, f));

        testReshapeMethod(chain0, expression0);
        testReshapeMethod(chainA, expressionA);
        testReshapeMethod(chainB, expressionB);
        testReshapeMethod(chainC, expressionC);
        testReshapeMethod(chainD, expressionD);
        testReshapeMethod(chainE, expressionE);
        testReshapeMethod(chainF, expressionF);
      });

    });

  });

});
