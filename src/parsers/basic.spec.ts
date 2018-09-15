/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { zip } from 'lodash';

import { PhraseExpression, TermExpression } from '../ast';
import { ParserConfig } from '../config';
import { basicExpression } from './basic';

const config = new ParserConfig();
const { AND, EXACT_MATCHER, GROUP_END, GROUP_START, OR } = config;

describe('SearchQL parsers', () => {

  const validInput = [
    '"ASDfas 32%@$%4512 u954anna as d][;];.{P} AND"',
    '"OR AND NOT (OR AND NOT) asd: asd not ASD:ASd"',
    '"  "',
    'asdANDas_NOTallalal',
    'shgfghjfhjfghs',
    '123123123',
    's#$%^876gsbjh_-s-S-s',
  ];

  const validOutput = [
    'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
    'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
    '  ',
    validInput[3],
    validInput[4],
    validInput[5],
    validInput[6],
  ].map(PhraseExpression.of);

  const invalidInput = [
    'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
    'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
    ' ',
    ...AND,
    ...OR,
    `123 ~= asd`,
    `asd : 23`,
    `${GROUP_START}asd${GROUP_END}`,
    `dasd${EXACT_MATCHER}a`,
  ];

  describe('basicExpression', () => {

    zip<any>(validInput, validOutput)
      .forEach(([input, output]) => {
        describe(`for valid input: '${input}'`, () => {
          const parsed = basicExpression(config).parse(input);

          it('should succeed', () => {
            expect(parsed.status).to.be.true;
          });

          it('should be evaluated to proper expression type', () => {
            expect(parsed.status ? parsed.value : null).to.be.instanceOf(TermExpression);
            expect(parsed.status ? parsed.value : null).to.be.instanceOf(PhraseExpression);
          });

          it('should provide proper value', () => {
            expect(parsed.status ? parsed.value : null).to.deep.equal(output);
          });

        });
      });

    invalidInput.forEach(input => {
      describe(`for invalid input: '${input}'`, () => {

        it('should fail', () => {
          expect(basicExpression(config).parse(input).status).to.be.false;
        });

      });
    });

  });

});
