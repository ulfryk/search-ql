/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';

import {
  AndOperator,
  BinaryOperationExpression,
  Expression,
  LikeOperator,
  NotExpression,
  OrOperator,
  SelectorExpression,
  TextExpression,
} from '../ast';
import { SyntaxConfig } from '../config';
import { ParserName } from './names';
import { QueryParserFactory } from './query-parser-factory';

const { AND, EXACT_MATCHER, GROUP_END, GROUP_START, LIKE, OR } = new SyntaxConfig();
const And = new AndOperator(AND[0]);
const Like = new LikeOperator(LIKE[0]);
const Or = new OrOperator(OR[0]);

const test = (
  validInput: string[],
  validOutput: Expression[],
  invalidInput: string[],
  parserNames: ParserName[],
) => {
  zip<any>(validInput, validOutput).forEach(([input, output]) => {
    describe(`for valid input: '${input}'`, () => {
      const parsed = new QueryParserFactory(parserNames).getParser().parse(input);

      it('should succeed', () => {
        expect(parsed.status).to.be.true;
      });

      it('should provide proper value', () => {
        expect(parsed.status && parsed.value.equals(output)).to.be.true;
        expect(parsed.status ? String(parsed.value) : null).to.equal(String(output));
      });

    });
  });

  invalidInput.forEach(input => {
    describe(`for invalid input: '${input}'`, () => {

      it('should fail', () => {
        expect(new QueryParserFactory(parserNames).getParser().parse(input).status).to.be.false;
      });

    });
  });
};

describe('SearchQL parsers', () => {

  describe('QueryParserFactory', () => {

    describe('with all parsers', () => {

      const allParserNames =
        [ParserName.Basic, ParserName.BinaryOperation, ParserName.Not];

      const validInput = [
        'aa',
        `a${AND[1]}`,
        `(((${OR[1]}a)))`,
        '"aa:bb AND cc dd"',
        'aa LIKE AA',
        'aa AND bb',
        '(aa AND bb)',
        // TODO: Add operator precedence
        // 'aa AND (bb LIKE BB OR cc) OR (dd AND (ee LIKE EE OR ff))',
        'aaa AND bbb OR ccc AND ddd',
        'NOT abc',
        'aa AND NOT bb',
        'aa NOT bb',
        'NOT aaa AND NOT bbb',
        'NOT (aaa OR bbb) AND ccc',
        // TODO: Add operator precedence
        // 'aaa AND NOT bb LIKE BB',
        'aaa AND (bbb OR ccc) NOT ddd',
        '(aaa OR bbb) NOT ccc',
        'aaa AND bbb AND ccc NOT ddd',
      ];

      const validOutput = [
        new TextExpression('aa'),
        new TextExpression(`a${AND[1]}`),
        new TextExpression(`${OR[1]}a`),
        new TextExpression('aa:bb AND cc dd'),
        new BinaryOperationExpression(Like, [
          new SelectorExpression('aa'),
          new TextExpression('AA'),
        ]),
        new BinaryOperationExpression(And, [
          new TextExpression('aa'),
          new TextExpression('bb'),
        ]),
        new BinaryOperationExpression(And, [
          new TextExpression('aa'),
          new TextExpression('bb'),
        ]),
        // TODO: Add operator precedence
        // new BinaryOperationExpression(Or, [
        //   new BinaryOperationExpression(And, [
        //     new TextExpression('aa'),
        //     new BinaryOperationExpression(Or, [
        //       new LabelledExpression('bb', new TextExpression('BB')),
        //       new TextExpression('cc'),
        //     ]),
        //   ]),
        //   new BinaryOperationExpression(And, [
        //     new TextExpression('dd'),
        //     new BinaryOperationExpression(Or, [
        //       new LabelledExpression('ee', new TextExpression('EE')),
        //       new TextExpression('ff'),
        //     ]),
        //   ]),
        // ]),
        new BinaryOperationExpression(And, [
          new BinaryOperationExpression(Or, [
            new BinaryOperationExpression(And, [
              new TextExpression('aaa'),
              new TextExpression('bbb'),
            ]),
            new TextExpression('ccc'),
          ]),
          new TextExpression('ddd'),
        ]),
        new NotExpression(new TextExpression('abc')),
        new BinaryOperationExpression(And, [
          new TextExpression('aa'),
          new NotExpression(new TextExpression('bb')),
        ]),
        new BinaryOperationExpression(And, [
          new TextExpression('aa'),
          new NotExpression(new TextExpression('bb')),
        ]),
        new BinaryOperationExpression(And, [
          new NotExpression(new TextExpression('aaa')),
          new NotExpression(new TextExpression('bbb')),
        ]),
        new BinaryOperationExpression(And, [
          new NotExpression(new BinaryOperationExpression(Or, [
            new TextExpression('aaa'),
            new TextExpression('bbb'),
          ])),
          new TextExpression('ccc'),
        ]),
        // TODO: Add operator precedence
        // new BinaryOperationExpression(And, [
        //   new TextExpression('aaa'),
        //   new NotExpression(new LabelledExpression('bb', new TextExpression('BB'))),
        // ]),
        new BinaryOperationExpression(And, [
          new BinaryOperationExpression(And, [
            new TextExpression('aaa'),
            new BinaryOperationExpression(Or, [
              new TextExpression('bbb'),
              new TextExpression('ccc'),
            ]),
          ]),
          new NotExpression(new TextExpression('ddd')),
        ]),
        new BinaryOperationExpression(And, [
          new BinaryOperationExpression(Or, [
            new TextExpression('aaa'),
            new TextExpression('bbb'),
          ]),
          new NotExpression(new TextExpression('ccc')),
        ]),
        new BinaryOperationExpression(And, [
          new BinaryOperationExpression(And, [
            new BinaryOperationExpression(And, [
              new TextExpression('aaa'),
              new TextExpression('bbb'),
            ]),
            new TextExpression('ccc'),
          ]),
          new NotExpression(new TextExpression('ddd')),
        ]),
      ];

      const invalidInput = [
        'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
        'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
        ' ',
        ...AND,
        ...OR,
        '123"asd',
        '"text',
        `${GROUP_START}asd`,
        `asd${GROUP_END}`,
        `dasd${EXACT_MATCHER}a`,
      ];

      test(validInput, validOutput, invalidInput, allParserNames);

    });

    describe('with some parsers excluded', () => {

      const subsetOfAllParsers = [ParserName.Basic, ParserName.BinaryOperation];

      const validInput = [
        'aa',
        'bb & cc',
      ];

      const validOutput = [
        new TextExpression('aa'),
        new BinaryOperationExpression(And, [
          new TextExpression('bb'),
          new TextExpression('cc'),
        ]),
      ];

      const invalidInput = [
        'desc abc AND',
        'NOT aaa',
        '! aaa',
      ];

      test(validInput, validOutput, invalidInput, subsetOfAllParsers);

    });

  });

});
