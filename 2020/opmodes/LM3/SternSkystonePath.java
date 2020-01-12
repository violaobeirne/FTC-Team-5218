package opmodes.LM3;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 1/7/2020.
 */

public class SternSkystonePath {

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

    public SternSkystonePath() {
        // BLUE ALLIANCE
        paths[SternSkystonePath.AllianceColor.BLUE.ordinal()] = new DeadReckonPath();


        // RED ALLIANCE
        paths[SternSkystonePath.AllianceColor.RED.ordinal()] = new DeadReckonPath();
    }

    public DeadReckonPath getPath(SternSkystonePath.AllianceColor allianceColor) {
        return paths[allianceColor.ordinal()];
    }

}
