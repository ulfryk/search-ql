/* tslint:disable:no-unused-expression no-magic-numbers */
import { expect } from 'chai';
import { Set } from 'immutable';
import * as _ from 'lodash';

import { BasicExpression, JoinedExpression } from '../ast';
import { SyntaxConfig } from '../syntax-config';
import { basicExpression, basicGroup } from './basic';

const config = new SyntaxConfig();
const { AND, EXACT_MATCHER, GROUP_END, GROUP_START, LABEL_DELIMITER, OR } = config;

describe('SearchQL parsers', () => {

  const validInput = [
    '"ASDfas 32%@$%4512 u954anna as d][;];.{P} AND"',
    '"OR AND NOT (OR AND NOT) asd: asd not ASD:ASd"',
    '" "',
    'asdANDas_NOTallalal',
    'shgfghjfhjfghs',
    '123123123',
    's#$%^876gsbjh_-s-S-s',
  ];

  const validOutput = [
    'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
    'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
    ' ',
    validInput[3],
    validInput[4],
    validInput[5],
    validInput[6],
  ].map(BasicExpression.fromMatch);

  const invalidInput = [
    'ASDfas 32%@$%4512 u954anna as d][;];.{P} AND',
    'OR AND NOT (OR AND NOT) asd: asd not ASD:ASd',
    ' ',
    AND,
    OR,
    `123${LABEL_DELIMITER}asd`,
    `asd${LABEL_DELIMITER}123`,
    `${GROUP_START}asd${GROUP_END}`,
    `dasd${EXACT_MATCHER}a`,
  ];

  describe('basicExpression', () => {

    _.zip<any>(validInput, validOutput).forEach(([input, output]) => {
      describe(`for valid input: '${input}'`, () => {
        const parsed = basicExpression(config).parse(input);

        it('should succeed', () => {
          expect(parsed.status).to.be.true;
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

  describe('basicGroup', () => {

    const g = (content: string) => [GROUP_START, content, GROUP_END].join('');

    const validGroups = [
      g('ispum'),
      g(validInput[0]),
      g(`lorem ${AND} ispum`),
      g(` ${validInput[0]} ${OR} ${validInput[1]} `),
      g(` \n lorem ${AND} ispum \n ${OR} ${validInput[0]} ${AND} dolor  ${AND} ${validInput[1]}`),
    ];

    const validGroupsOutput = [
      new BasicExpression('ispum'),
      validOutput[0],
      new JoinedExpression(AND, Set([
        new BasicExpression('lorem'),
        new BasicExpression('ispum'),
      ])),
      new JoinedExpression(OR, Set([
        validOutput[0],
        validOutput[1],
      ])),
      new JoinedExpression(AND, Set([
        new JoinedExpression(OR, Set([
          new JoinedExpression(AND, Set([
            new BasicExpression('lorem'),
            new BasicExpression('ispum'),
          ])),
          validOutput[0],
        ])),
        new BasicExpression('dolor'),
        validOutput[1],
      ])),
    ];

    const invalidGroups = [
      g(`lorem ipsum`),
      g(`lorem ${AND}`),
      g(`lorem${AND} ispum`),
      g(`dolor ${validInput[0]}`),
      g(`dolor${validInput[0]}`),
      g(`dolor ${AND}${validInput[0]}`),
    ].concat(invalidInput.map(g));

    _.zip<any>(validGroups, validGroupsOutput).forEach(([input, output]) => {
      describe(`for valid input: ${input}`, () => {
        const parsed = basicGroup(config).parse(input);

        it('should succeed', () => {
          expect(parsed.status).to.be.true;
        });

        it('should provide proper value', () => {
          expect(parsed.status && parsed.value.equals(output)).to.be.true;
        });

      });
    });

    invalidGroups.forEach(input => {
      describe(`for invalid input: '${input}'`, () => {

        it('should fail', () => {
          expect(basicExpression(config).parse(input).status).to.be.false;
        });

      });
    });

  });

});
