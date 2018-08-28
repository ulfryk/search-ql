/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';
import { Right } from 'monet';
import { Failure } from 'parsimmon';

import { AndOperator, BinaryOperationExpression, Expression, LikeOperator, OrOperator, SelectorExpression, TextExpression } from './ast';
import { SyntaxConfig } from './config';
import { parseSearchQL } from './parse-search-ql';
import { ParserName } from './parsers';

describe('SearchQL', () => {

  describe('parseSearchQL with regular config', () => {

    const { AND, LIKE } = new SyntaxConfig();
    const And = new AndOperator(AND[0]);
    const Like = new LikeOperator(LIKE[0]);
    // const Or = new OrOperator(OR[0]);

    const allParserNames =
      [ParserName.Basic, ParserName.BinaryOperation, ParserName.Not];

    const validInput = [
      'aaa AND bbb',
      'token_expired LIKE true',
      // TODO: Add operator precedence
      // 'first_name LIKE Adam OR token_expired LIKE true',
    ];
    const successfulOutputValues = [
      Right<Failure, Expression>(new BinaryOperationExpression(And, [
        new TextExpression('aaa'),
        new TextExpression('bbb'),
      ])),
      Right<Failure, Expression>(new BinaryOperationExpression(Like, [
        new SelectorExpression('token_expired'),
        new TextExpression('true'),
      ])),
      // TODO: Add operator precedence
      // Right<Failure, Expression>(new BinaryOperationExpression(Or, [
      //   new BinaryOperationExpression(Like, [
      //     new SelectorExpression('first_name'),
      //     new TextExpression('Adam'),
      //   ]),
      //   new BinaryOperationExpression(Like, [
      //     new SelectorExpression('token_expired'),
      //     new TextExpression('true'),
      //   ]),
      // ])),
    ];

    const invalidInput = [
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
      'asdasd OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
      ' ',
      'des:: c: ipsum OR NOT dolor AND "john doe":"',
    ];

    zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
      describe(`for valid input: ${input}`, () => {
        const parsed = parseSearchQL(allParserNames)(input);

        it('should return Right', () => {
          expect(parsed.isRight()).to.be.true;
        });

        it('should return parsed expression', () => {
          expect(parsed.equals(output)).to.be.true;
        });

      });
    });

    invalidInput.forEach(input => {
      describe(`for invalid input: ${input}`, () => {
        const parsed = parseSearchQL(allParserNames)(input);

        it('should return Left', () => {
          expect(parsed.isLeft()).to.be.true;
        });

        it('should provide original input', () => {
          expect(parsed.cata(({ query }) => query, () => null)).to.equal(input);
        });

      });
    });

  });

  describe('parseSearchQL with custom config', () => {

    const config = SyntaxConfig.create({ AND: ['&&'], OR: ['||'], LIKE: ['~='] });

    const { AND, LIKE, OR } = config;
    const And = new AndOperator(AND[0]);
    const Like = new LikeOperator(LIKE[0]);
    const Or = new OrOperator(OR[0]);

    const allParserNames =
      [ParserName.Basic, ParserName.BinaryOperation, ParserName.Not];

    const validInput = [
      'aaa && bbb',
      'aaa || bbb',
      'first_name ~= Adam',
      // TODO: Add operator precedence
      // 'first_name ~= Adam && token_expired ~= true',
    ];
    const successfulOutputValues = [
      Right<Failure, Expression>(new BinaryOperationExpression(And, [
        new TextExpression('aaa'),
        new TextExpression('bbb'),
      ])),
      Right<Failure, Expression>(new BinaryOperationExpression(Or, [
        new TextExpression('aaa'),
        new TextExpression('bbb'),
      ])),
      Right<Failure, Expression>(new BinaryOperationExpression(Like, [
        new SelectorExpression('first_name'),
        new TextExpression('Adam'),
      ])),
      // TODO: Add operator precedence
      // Right<Failure, Expression>(new BinaryOperationExpression(And, [
      //   new BinaryOperationExpression(Like, [
      //     new SelectorExpression('first_name'),
      //     new TextExpression('Adam'),
      //   ]),
      //   new BinaryOperationExpression(Like, [
      //     new SelectorExpression('token_expired'),
      //     new TextExpression('true'),
      //   ]),
      // ])),
    ];

    const invalidInput = [
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} &&',
      'asdasd || && NOT (|| && NOT) asd~ asd not ASD~ASd',
      ' ',
      'des~~ c~ ipsum || NOT dolor && "john doe"~"',
    ];

    zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
      describe(`for valid input: ${input}`, () => {
        const parsed = parseSearchQL(allParserNames, config)(input);

        it('should return Right', () => {
          expect(parsed.isRight()).to.be.true;
        });

        it('should return parsed expression', () => {
          expect(parsed.equals(output)).to.be.true;
        });

      });
    });

    invalidInput.forEach(input => {
      describe(`for invalid input: ${input}`, () => {
        const parsed = parseSearchQL(allParserNames, config)(input);

        it('should return Left', () => {
          expect(parsed.isLeft()).to.be.true;
        });

        it('should provide original input', () => {
          expect(parsed.cata(({ query }) => query, () => null)).to.equal(input);
        });

      });
    });

  });

});
