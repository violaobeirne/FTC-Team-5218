package opmodes.ILT;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 1/7/2020.
 */

public class BeethovenILTSkystonePath {

    public enum ArmLocation {
        ARM_STOWED,
        ARM_DEPLOYED,
    }
    public enum AllianceColor {
        BLUE,
        RED,
        DEFAULT;
    }

    DeadReckonPath[] paths = new DeadReckonPath[2];

    public BeethovenILTSkystonePath() {
        // BLUE ALLIANCE
        paths[BeethovenILTSkystonePath.AllianceColor.BLUE.ordinal()] = new DeadReckonPath();


        // RED ALLIANCE
        paths[BeethovenILTSkystonePath.AllianceColor.RED.ordinal()] = new DeadReckonPath();
    }

    public DeadReckonPath getPath(BeethovenILTSkystonePath.AllianceColor allianceColor) {
        return paths[allianceColor.ordinal()];
    }

}
