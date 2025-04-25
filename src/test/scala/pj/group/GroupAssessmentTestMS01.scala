package pj.group

import pj.assessment.{AssessmentBehaviours, AssessmentMS01}

class GroupAssessmentTestMS01 extends AssessmentBehaviours:
  performAllTests(AssessmentMS01.create, "files/group/ms01", "Milestone 1")
