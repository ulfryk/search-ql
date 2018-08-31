/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';
import { Either } from 'monet';

import { AndOperator, LikeOperator, OrOperator } from './ast';
import { SyntaxConfig } from './config';
import { parseSearchQL } from './parse-search-ql';
import { ParserName } from './parsers';
import { and, fn, like, or, txt } from './testing/utils';

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
      'test_function(aaa, bbb)',
    ];

    const successfulOutputValues = [
      and(txt('aaa'), txt('bbb')),
      like(txt('token_expired'), txt('true')),
      or(like(txt('first_name'), txt('Adam')), like(txt('token_expired'), txt('true'))),
      fn('test_function')(txt('aaa'), txt('bbb')),
    ].map(Either.of);

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
          expect(parsed.cata(({ query }) => query, () => null)).to.equal(input);
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
      and(txt('aaa'), txt('bbb'), AndC),
      or(txt('aaa'), txt('bbb'), OrC),
      like(txt('first_name'), txt('Adam'), LikeC),
      and(
        like(txt('first_name'), txt('Adam'), LikeC),
        like(txt('token_expired'), txt('true'), LikeC),
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
          expect(parsed.cata(({ query }) => query, () => null)).to.equal(input);
        });

      });
    });

  });

});
