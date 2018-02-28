/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Set } from 'immutable';
import * as _ from 'lodash';

import {
  BasicExpression,
  Expression,
  JoinedExpression,
  LabelledExpression,
  NotExpression,
} from '../expressions';
import { AND, EXACT_MATCHER, GROUP_END, GROUP_START, LABEL_DELIMITER, OR } from '../syntax-config';
import { ParserName } from './names';
import { query } from './query';

const test = (
  validInput: string[],
  validOutput: Expression[],
  invalidInput: string[],
  parserNames: ParserName[],
) => {
  _.zip<any>(validInput, validOutput).forEach(([input, output]) => {
    describe(`for valid input: '${input}'`, () => {
      const parsed = query(parserNames).parse(input);

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
        expect(query(parserNames).parse(input).status).to.be.false;
      });

    });
  });
};

describe('SearchQL parsers', () => {

  describe('query', () => {

    describe('with all parsers', () => {

      const allParserNames =
        [ParserName.Basic, ParserName.JoinedGroup, ParserName.Labelled, ParserName.Not];

      const validInput = [
        'aa',
        'aa AND',
        'aa OR',
        'aa AND ',
        'aa OR ',
        'aa OR   ',
        `a${AND}`,
        `(((${OR}a)))`,
        '"aa:bb AND cc dd"',
        'aa : AA',
        'aa AND bb',
        'aa AND bb AND',
        '(aa AND bb)',
        'aa AND (bb:BB OR cc) OR (dd AND (ee:EE OR ff))',
        'aaa AND bbb OR ccc AND ddd',
        'NOT abc',
        'aa AND NOT bb',
        'aa NOT bb',
        'NOT aaa AND NOT bbb',
        'NOT (aaa OR bbb) AND ccc',
        'aaa AND NOT bb:BB',
        'aaa AND (bbb OR ccc) NOT ddd',
        '(aaa OR bbb) NOT ccc',
        'aaa AND bbb AND ccc NOT ddd',
      ];

      const validOutput = [
        new BasicExpression('aa'),
        new BasicExpression('aa'),
        new BasicExpression('aa'),
        new BasicExpression('aa'),
        new BasicExpression('aa'),
        new BasicExpression('aa'),
        new BasicExpression(`a${AND}`),
        new BasicExpression(`${OR}a`),
        new BasicExpression('aa:bb AND cc dd'),
        new LabelledExpression('aa', new BasicExpression('AA')),
        new JoinedExpression(AND, Set([
          new BasicExpression('aa'),
          new BasicExpression('bb'),
        ])),
        new JoinedExpression(AND, Set([
          new BasicExpression('aa'),
          new BasicExpression('bb'),
        ])),
        new JoinedExpression(AND, Set([
          new BasicExpression('aa'),
          new BasicExpression('bb'),
        ])),
        new JoinedExpression(OR, Set([
          new JoinedExpression(AND, Set([
            new BasicExpression('aa'),
            new JoinedExpression(OR, Set([
              new LabelledExpression('bb', new BasicExpression('BB')),
              new BasicExpression('cc'),
            ])),
          ])),
          new JoinedExpression(AND, Set([
            new BasicExpression('dd'),
            new JoinedExpression(OR, Set([
              new LabelledExpression('ee', new BasicExpression('EE')),
              new BasicExpression('ff'),
            ])),
          ])),
        ])),
        new JoinedExpression(AND, Set([
          new JoinedExpression(OR, Set([
            new JoinedExpression(AND, Set([
              new BasicExpression('aaa'),
              new BasicExpression('bbb'),
            ])),
            new BasicExpression('ccc'),
          ])),
          new BasicExpression('ddd'),
        ])),
        new NotExpression(new BasicExpression('abc')),
        new JoinedExpression(AND, Set([
          new BasicExpression('aa'),
          new NotExpression(new BasicExpression('bb')),
        ])),
        new JoinedExpression(AND, Set([
          new BasicExpression('aa'),
          new NotExpression(new BasicExpression('bb')),
        ])),
        new JoinedExpression(AND, Set([
          new NotExpression(new BasicExpression('aaa')),
          new NotExpression(new BasicExpression('bbb')),
        ])),
        new JoinedExpression(AND, Set([
          new NotExpression(new JoinedExpression(OR, Set([
            new BasicExpression('aaa'),
            new BasicExpression('bbb'),
          ]))),
          new BasicExpression('ccc'),
        ])),
        new JoinedExpression(AND, Set([
          new BasicExpression('aaa'),
          new NotExpression(new LabelledExpression('bb', new BasicExpression('BB'))),
        ])),
        new JoinedExpression(AND, Set([
          new JoinedExpression(AND, Set([
            new BasicExpression('aaa'),
            new JoinedExpression(OR, Set([
              new BasicExpression('bbb'),
              new BasicExpression('ccc'),
            ])),
          ])),
          new NotExpression(new BasicExpression('ddd')),
        ])),
        new JoinedExpression(AND, Set([
          new JoinedExpression(OR, Set([
            new BasicExpression('aaa'),
            new BasicExpression('bbb'),
          ])),
          new NotExpression(new BasicExpression('ccc')),
        ])),
        new JoinedExpression(AND, Set([
          new JoinedExpression(AND, Set([
            new BasicExpression('aaa'),
            new BasicExpression('bbb'),
            new BasicExpression('ccc'),
          ])),
          new NotExpression(new BasicExpression('ddd')),
        ])),
      ];

      const invalidInput = [
        'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
        'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
        ' ',
        AND,
        OR,
        `123${LABEL_DELIMITER}a${LABEL_DELIMITER}sd`,
        `${LABEL_DELIMITER}text`,
        `${GROUP_START}asd`,
        `asd${GROUP_END}`,
        `dasd${EXACT_MATCHER}a`,
      ];

      test(validInput, validOutput, invalidInput, allParserNames);

    });

    describe('with some parsers excluded', () => {

      const subsetOfAllParsers = [ParserName.Basic, ParserName.JoinedGroup];

      const validInput = [
        'aa',
      ];

      const validOutput = [
        new BasicExpression('aa'),
      ];

      const invalidInput = [
        'desc:abc',
        'NOT aaa',
      ];

      test(validInput, validOutput, invalidInput, subsetOfAllParsers);

    });

  });

});
