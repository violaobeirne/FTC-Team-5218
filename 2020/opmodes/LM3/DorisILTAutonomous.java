package opmodes.LM3;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.RobotEvent;
import team25core.StandardFourMotorRobot;

/**
 * Created by Lizzie on 11/2/2019.
 */
@Autonomous(name = "dOrIS Autonomous")
public class DorisILTAutonomous extends StandardFourMotorRobot {

    // drivetrain and mechanisms declaration
    private DcMotor leftIntake;
    private DcMotor rightIntake;
    private Servo leftArm;
    private Servo rightArm;
    private Servo leftStoneArm;
    private Servo rightStoneArm;
    private MechanumGearedDrivetrain drivetrain;

    // gamepad and telemetry declaration
    private GamepadTask gamepad;
    private Telemetry.Item alliance;
    private Telemetry.Item startPos;
    private Telemetry.Item path;

    // skybridge constant declaration
    private DeadReckonPath moveFoundationPath;
    private DorisSkybridgePath skybridgePath;
    private DorisSkybridgePath.AllianceColor allianceColor;
    private DorisSkybridgePath.StartingPosition startingPosition;
    private DorisSkybridgePath.ArmLocation foundationArms;
    private DorisSkybridgePath.ArmLocation stoneArms;

    @Override
    public void init() {
        super.init();
        leftIntake = hardwareMap.dcMotor.get("leftIntake");
        rightIntake = hardwareMap.dcMotor.get("rightIntake");

        leftArm = hardwareMap.servo.get("leftArm");
        rightArm = hardwareMap.servo.get("rightArm");
        leftStoneArm = hardwareMap.servo.get("leftStoneArm");
        rightStoneArm = hardwareMap.servo.get("rightStoneArm");

        drivetrain = new MechanumGearedDrivetrain(motorMap);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();
        drivetrain.setMasterMotor(frontLeft);

        // gamepad and telemetry initialization
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        alliance = telemetry.addData("Alliance: ", "NOT SELECTED");
        startPos = telemetry.addData("Starting Position: ", "NOT SELECTED");
        path = telemetry.addData("Path: ", "NOT SELECTED");

        skybridgePath = new DorisSkybridgePath();
        moveFoundationPath = new DeadReckonPath();
        allianceColor = allianceColor.DEFAULT;
        startingPosition = startingPosition.DEFAULT;
        leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
        rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
        foundationArms = DorisSkybridgePath.ArmLocation.ARM_STOWED;
        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
        stoneArms = DorisSkybridgePath.ArmLocation.ARM_STOWED;
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            handleGamePadSelection(event);
        }
    }

    @Override
    public void start() {
        leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
        rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
        moveFoundationPath = skybridgePath.getPath(allianceColor, startingPosition);
        moveFoundation(moveFoundationPath);
    }

    public void handleGamePadSelection (GamepadTask.GamepadEvent event) {
        switch (event.kind){
            case BUTTON_X_DOWN:
                allianceColor = allianceColor.BLUE;
                alliance.setValue("BLUE");
                break;
            case BUTTON_B_DOWN:
                allianceColor = allianceColor.RED;
                alliance.setValue("RED");
                break;
            case BUTTON_Y_DOWN:
                startingPosition = startingPosition.BUILDING;
                startPos.setValue("BUILDING SIDE");
                break;
            case BUTTON_A_DOWN:
                startingPosition = startingPosition.LOADING;
                startPos.setValue("LOADING SIDE");
                break;
        }
    }

    public void moveFoundation(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        RobotLog.i("163: PATH DONE");
                        break;
                    case PAUSING:
                        RobotLog.i("163: SEGMENT DONE %d", num);
                        if (startingPosition == DorisSkybridgePath.StartingPosition.BUILDING) {
                            moveFoundationArms();
                        } else if (startingPosition == DorisSkybridgePath.StartingPosition.LOADING) {
                            moveStoneArms();
                        }
                        break;
                    /*
                    case SEGMENT_DONE:
                        RobotLog.i("163: SEGMENT DONE %d", num);
                        if (num == 2) {
                        } else if (num == 5) {
                           if (startingPosition == DorisSkybridgePath.StartingPosition.BUILDING) {
                               moveFoundationArms();
                           } else if (startingPosition == DorisSkybridgePath.StartingPosition.LOADING) {
                               moveStoneArms();
                           }
                        }
                        break;
                    */
                }
            }
        });
    }

    public void moveFoundationArms()
    {
        switch (foundationArms) {
            case ARM_DEPLOYED:
                leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
                rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
                foundationArms = DorisSkybridgePath.ArmLocation.ARM_STOWED;
                break;
            case ARM_STOWED:
                leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_DOWN);
                rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_DOWN);
                foundationArms = DorisSkybridgePath.ArmLocation.ARM_DEPLOYED;
                break;
        }
    }

    public void moveStoneArms ()
    {
        switch (stoneArms) {
            case ARM_DEPLOYED:
                leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_DOWN);
                rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_DOWN);
                stoneArms = DorisSkybridgePath.ArmLocation.ARM_STOWED;
                break;
            case ARM_STOWED:
                leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
                rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
                stoneArms = DorisSkybridgePath.ArmLocation.ARM_DEPLOYED;
                break;
        }
    }
}
