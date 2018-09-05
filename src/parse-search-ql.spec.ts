/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';
import { Either } from 'monet';

import { AndOperator, LikeOperator, OrOperator } from './ast';
import { ParseFailure } from './common/model';
import { SyntaxConfig } from './config';
import { parseSearchQL } from './parse-search-ql';
import { ParserName } from './parsers';
import { and, fn, like, or, term, txt } from './testing/utils';

const allParserNames = [
  ParserName.Basic,
  ParserName.BinaryOperation,
  ParserName.Function,
  ParserName.Not,
];

describe('SearchQL', () => {

  describe('parseSearchQL with regular config', () => {

    const validInput = [
      'aaa & bbb',
      'token_expired ~ true',
      'first_name ~ Adam | token_expired ~ true',
      'test_function(aaa, "b(b & b)")',
    ];

    const successfulOutputValues = [
      and(term('aaa'), term('bbb')),
      like(txt('token_expired'), term('true')),
      or(like(txt('first_name'), term('Adam')), like(txt('token_expired'), term('true'))),
      fn('test_function')(txt('aaa'), txt('b(b & b)')),
    ].map(Either.of);

    const invalidInput = [
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
      'asdasd OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
      ' ',
      'des:: c: ipsum OR NOT dolor AND "john doe":"',
    ];

    const invalidTypesInput = [
      'test_function(aaa, (token_expired ~ true))',
      'test_function(test_function(aaa), (token_expired ~ true), (! ccc))',
      'test_function(aaa, bbb, (! ccc)) ~ (! "John Doe")',
    ];

    const invalidTypesOutput = [
      ['TypeError: Function "test_function" has wrong 2nd rest arg passed, should be TEXT but is BOOLEAN'],
      [
        'TypeError: Function "test_function" has wrong 1st rest arg passed, should be TEXT but is BOOLEAN',
        'TypeError: Function "test_function" has wrong 2nd rest arg passed, should be TEXT but is BOOLEAN',
        'TypeError: Function "test_function" has wrong 3rd rest arg passed, should be TEXT but is BOOLEAN',
      ],
      [
        'TypeError: LHS of ~ expression has to be a TEXT expression, but instead found BOOLEAN',
        'TypeError: Function "test_function" has wrong 3rd rest arg passed, should be TEXT but is BOOLEAN',
      ],
    ];

    zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
      describe(`for valid input: ${input}`, () => {
        const parsed = parseSearchQL(allParserNames)(input);

        it('should return Right', () => {
          expect(parsed.isRight(), parsed.cata(String, String)).to.be.true;
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
          expect(parsed.cata(
            f => f.map(({ query }: ParseFailure) => query)[0],
            () => null)).to.equal(input);
        });

      });
    });

    invalidTypesInput.forEach((input, i) => {
      describe(`for syntactically valid yet wrongly typed input: ${input}`, () => {
        const parsed = parseSearchQL(allParserNames)(input);

        it('should return Left', () => {
          expect(parsed.isLeft(), parsed.cata(String, String)).to.be.true;
        });

        it('should yield proper type error information', () => {
          expect(parsed.left().map(String)).to.deep.equal(invalidTypesOutput[i]);
        });

      });
    });

  });

  describe('parseSearchQL with custom config', () => {

    const configC = SyntaxConfig.create({
      AND: ['&&'],
      FN_ARG_SEPARATOR: '|',
      FN_LEFT_PAREN: '/',
      FN_RIGHT_PAREN: '/',
      LIKE: ['~='],
      OR: ['||'],
    });

    const AndC = new AndOperator('&&');
    const LikeC = new LikeOperator('||');
    const OrC = new OrOperator('~=');

    const validInput = [
      'aaa && bbb',
      '(aaa || bbb)',
      'first_name ~= Adam',
      'first_name ~= Adam && token_expired ~= true',
      'test_function/    aaa|bbb/',
    ];

    const successfulOutputValues = [
      and(term('aaa'), term('bbb'), AndC),
      or(term('aaa'), term('bbb'), OrC),
      like(txt('first_name'), term('Adam'), LikeC),
      and(
        like(txt('first_name'), term('Adam'), LikeC),
        like(txt('token_expired'), term('true'), LikeC),
        AndC),
      fn('test_function')(txt('aaa'), txt('bbb')),
    ].map(Either.of);

    const invalidInput = [
      'aa&aa',
      'aa|aa',
      'aa~aa',
      'aa=aa',
      'aa/aa',
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} &&',
      'asdasd || && NOT (|| && NOT) asd~ asd not ASD~ASd',
      ' ',
      'des~~ c~ ipsum || NOT dolor && "john doe"~"',
      'test_function/aaa|bbb\\',
    ];

    zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
      describe(`for valid input: ${input}`, () => {
        const parsed = parseSearchQL(allParserNames, configC)(input);

        it('should return Right', () => {
          expect(parsed.isRight(), parsed.cata(String, String)).to.be.true;
        });

        it('should return parsed expression', () => {
          expect(parsed.equals(output)).to.be.true;
        });

      });
    });

    invalidInput.forEach(input => {
      describe(`for invalid input: ${input}`, () => {
        const parsed = parseSearchQL(allParserNames, configC)(input);

        it('should return Left', () => {
          expect(parsed.isLeft()).to.be.true;
        });

        it('should provide original input', () => {
          expect(parsed.cata(
            f => f.map(({ query }: ParseFailure) => query)[0],
            () => null)).to.equal(input);
        });

      });
    });

  });

});
