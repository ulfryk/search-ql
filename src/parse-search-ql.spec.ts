/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Set } from 'immutable';
import * as _ from 'lodash';
import { Right } from 'monet';
import { Failure } from 'parsimmon';

import { BasicExpression, Expression, JoinedExpression, LabelledExpression } from './ast';
import { parseSearchQL } from './parse-search-ql';
import { ParserName } from './parsers';
import { SyntaxConfig } from './syntax-config';

describe('SearchQL', () => {

  describe('parseSearchQL with regular config', () => {

    const { AND } = new SyntaxConfig();

    const allParserNames =
      [ParserName.Basic, ParserName.JoinedGroup, ParserName.Labelled, ParserName.Not];

    const validInput = [
      'aaa AND bbb',
      'first_name:Adam AND token_expired:true',
    ];
    const successfulOutputValues = [
      Right<Failure, Expression>(new JoinedExpression(AND, Set([
        new BasicExpression('aaa'),
        new BasicExpression('bbb'),
      ]))),
      Right<Failure, Expression>(new JoinedExpression(AND, Set([
        new LabelledExpression('first_name', new BasicExpression('Adam')),
        new LabelledExpression('token_expired', new BasicExpression('true')),
      ]))),
    ];

    const invalidInput = [
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
      'asdasd OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
      ' ',
      'des:: c: ipsum OR NOT dolor AND "john doe":"',
    ];

    _.zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
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

    const config = SyntaxConfig.create({ AND: '&&', OR: '||', LABEL_DELIMITER: '~' });
    const { AND, OR } = config;

    const allParserNames =
      [ParserName.Basic, ParserName.JoinedGroup, ParserName.Labelled, ParserName.Not];

    const validInput = [
      'aaa && bbb',
      'aaa || bbb',
      'first_name ~ Adam && token_expired ~ true',
    ];
    const successfulOutputValues = [
      Right<Failure, Expression>(new JoinedExpression(AND, Set([
        new BasicExpression('aaa'),
        new BasicExpression('bbb'),
      ]))),
      Right<Failure, Expression>(new JoinedExpression(OR, Set([
        new BasicExpression('aaa'),
        new BasicExpression('bbb'),
      ]))),
      Right<Failure, Expression>(new JoinedExpression(AND, Set([
        new LabelledExpression('first_name', new BasicExpression('Adam')),
        new LabelledExpression('token_expired', new BasicExpression('true')),
      ]))),
    ];

    const invalidInput = [
      'ASDfas 32%@$%4512 u954anna as d][;];.{P} &&',
      'asdasd || && NOT (|| && NOT) asd~ asd not ASD~ASd',
      ' ',
      'des~~ c~ ipsum || NOT dolor && "john doe"~"',
    ];

    _.zip<any>(validInput, successfulOutputValues).forEach(([input, output]) => {
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
