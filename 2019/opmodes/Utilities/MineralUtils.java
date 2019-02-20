package opmodes;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import static opmodes.MineralUtils.MineralPosition.UNKNOWN;
import static team25core.MineralDetectionTask.LABEL_GOLD_MINERAL;

/**
 * Created by Lizzie on 1/19/2019.
 */
public class MineralUtils {

    public enum MineralPosition {
        LEFT,
        RIGHT,
        CENTER,
        UNKNOWN,
    }

    public enum DropMarker {
        TRUE,
        FALSE,
        DEFAULT,
    }

    public static void sendPositionTelemetry(MineralPosition pos, Telemetry.Item item) {
        switch (pos) {
            case LEFT:
                item.setValue("LEFT");
                break;
            case RIGHT:
                item.setValue("RIGHT");
                break;
            case CENTER:
                item.setValue("CENTER");
                break;
        }
    }

    public static MineralPosition determineGoldPosition(List<Recognition> minerals) {
        if (minerals != null) {
            int goldMineralX = -1;
            int silverMineral1X = -1;
            int silverMineral2X = -1;
            for (Recognition recognition : minerals) {
                if (recognition.getLabel().equals(LABEL_GOLD_MINERAL)) {
                    goldMineralX = (int) recognition.getLeft();
                } else if (silverMineral1X == -1) {
                    silverMineral1X = (int) recognition.getLeft();
                } else {
                    silverMineral2X = (int) recognition.getLeft();
                }
            }


            // two mineral code
            if (goldMineralX != -1 && silverMineral1X != -1) {
                if (goldMineralX < silverMineral1X) {
                    return MineralPosition.CENTER;
                } else if (goldMineralX > silverMineral1X) {
                    return MineralPosition.RIGHT;
                }
            } else if (silverMineral1X != -1 && silverMineral2X != -1) {
                return MineralPosition.LEFT;
            }
            /*
            // three mineral code
            if (goldMineralX != -1 && silverMineral1X != -1 && silverMineral2X != -1) {
                if (goldMineralX < silverMineral1X && goldMineralX < silverMineral2X) {
                    return MineralPosition.LEFT;
                } else if (goldMineralX > silverMineral1X && goldMineralX > silverMineral2X) {
                    return MineralPosition.RIGHT;
                } else {
                    return MineralPosition.CENTER;
                }
            }
            */

        }
        return UNKNOWN;
    }



}
